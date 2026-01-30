# HTTP Infrastructure with httr2
# Provides reliable API requests with retry, throttle, and error handling

# Error Classes ---------------------------------------------------------------

#' Create a realtR API error
#' @param message Error message
#' @param status HTTP status code
#' @param url The URL that failed
#' @keywords internal
#' @noRd
.realtr_api_error <- function(message, status = NULL, url = NULL) {
  rlang::abort(
    message = message,
    class = c("RealtrApiError", "error"),
    status = status,
    url = url
  )
}

#' Create a realtR rate limit error
#' @keywords internal
#' @noRd
.realtr_rate_limit_error <- function(url = NULL, retry_after = NULL) {
  msg <- "Rate limited by realtor.com. Try again later."


  if (!is.null(retry_after)) {
    msg <- glue::glue("Rate limited. Retry after {retry_after} seconds.")
  }

  rlang::abort(
    message = msg,
    class = c("RealtrRateLimitError", "RealtrApiError", "error"),
    url = url,
    retry_after = retry_after
  )
}

#' Create a realtR not found error
#' @keywords internal
#' @noRd
.realtr_not_found_error <- function(url = NULL) {
  rlang::abort(
    message = "Resource not found (404). The endpoint may have changed.",
    class = c("RealtrNotFoundError", "RealtrApiError", "error"),
    url = url
  )
}

# HTTP Request Helpers --------------------------------------------------------

#' Check if HTTP status is transient (retryable)
#' @param status HTTP status code
#' @keywords internal
#' @noRd
.is_transient_error <- function(status) {
  status %in% c(429L, 500L, 502L, 503L, 504L)
}

#' Handle HTTP response errors
#' @param resp httr2 response object
#' @param url Original URL
#' @keywords internal
#' @noRd
.handle_response_error <- function(resp, url) {
  status <- httr2::resp_status(resp)

  if (status == 429L) {
    retry_after <- httr2::resp_header(resp, "Retry-After")
    .realtr_rate_limit_error(url = url, retry_after = retry_after)
  } else if (status == 404L) {
    .realtr_not_found_error(url = url)
  } else if (status >= 400L) {
    .realtr_api_error(
      message = glue::glue(
        "API request failed with status {status}: {httr2::resp_status_desc(resp)}"
      ),
      status = status,
      url = url
    )
  }
}

# Core HTTP Functions ---------------------------------------------------------

#' Fetch a page using httr2 with retry and throttle
#'
#' Replacement for .curl_page() with automatic retry on transient errors,
#' rate limiting, and user-agent rotation.
#'
#' @param url URL to fetch
#' @param max_tries Maximum retry attempts (default: 3)
#' @keywords internal
#' @noRd
.fetch_page <- function(url, max_tries = 3L) {
  df_call <- generate_url_reference()

  resp <- httr2::request(url) |>
    httr2::req_user_agent(df_call$userAgent) |>
    httr2::req_headers(
      Referer = df_call$urlReferer,
      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      `Accept-Language` = "en-US,en;q=0.5"
    ) |>
    httr2::req_retry(
      max_tries = max_tries,
      is_transient = \(resp) .is_transient_error(httr2::resp_status(resp)),
      backoff = \(i) stats::runif(1, min = 1, max = 2^i)
    ) |>
    httr2::req_throttle(
      rate = 30 / 60,  # 30 requests per minute
      realm = "realtor.com"
    ) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  # Check for errors after retry attempts
  if (httr2::resp_is_error(resp)) {
    .handle_response_error(resp, url)
  }

  # Parse as HTML
  httr2::resp_body_html(resp)
}

#' Fetch JSON using httr2 with retry and throttle
#'
#' Replacement for .curl_json() with automatic retry on transient errors,
#' rate limiting, and user-agent rotation.
#'
#' @param url URL to fetch
#' @param max_tries Maximum retry attempts (default: 3)
#' @keywords internal
#' @noRd
.fetch_json <- function(url, max_tries = 3L) {
  df_call <- generate_url_reference()

  resp <- httr2::request(url) |>
    httr2::req_user_agent(df_call$userAgent) |>
    httr2::req_headers(
      Referer = df_call$urlReferer,
      Accept = "application/json",
      `Accept-Language` = "en-US,en;q=0.5"
    ) |>
    httr2::req_retry(
      max_tries = max_tries,
      is_transient = \(resp) .is_transient_error(httr2::resp_status(resp)),
      backoff = \(i) stats::runif(1, min = 1, max = 2^i)
    ) |>
    httr2::req_throttle(
      rate = 30 / 60,  # 30 requests per minute
      realm = "realtor.com"
    ) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  # Check for errors after retry attempts
  if (httr2::resp_is_error(resp)) {
    .handle_response_error(resp, url)
  }

  # Return raw text (matching .curl_json behavior for compatibility)
  httr2::resp_body_string(resp)
}

#' Fetch JSON and parse directly
#'
#' Convenience wrapper that fetches and parses JSON in one step.
#'
#' @param url URL to fetch
#' @param max_tries Maximum retry attempts (default: 3)
#' @keywords internal
#' @noRd
.fetch_json_parsed <- function(url, max_tries = 3L) {
  json_text <- .fetch_json(url, max_tries = max_tries)
  jsonlite::fromJSON(json_text, simplifyVector = TRUE)
}

# Configuration ---------------------------------------------------------------

#' Configure realtR HTTP behavior
#'
#' Set global options for HTTP requests including rate limiting and retry behavior.
#'
#' @param rate_limit Requests per minute (default: 30)
#' @param max_retries Maximum retry attempts (default: 3)
#' @param user_agent_rotation Enable user-agent rotation (default: TRUE)
#'
#' @return Invisibly returns previous options
#' @export
#'
#' @examples
#' \dontrun{
#' # Reduce rate limit for conservative crawling
#' realtr_config(rate_limit = 10)
#'
#' # Increase retries for flaky connections
#' realtr_config(max_retries = 5)
#' }
realtr_config <- function(rate_limit = 30,
                          max_retries = 3,
                          user_agent_rotation = TRUE) {
  old <- list(
    rate_limit = getOption("realtR.rate_limit", 30),
    max_retries = getOption("realtR.max_retries", 3),
    user_agent_rotation = getOption("realtR.user_agent_rotation", TRUE)
  )

  options(
    realtR.rate_limit = rate_limit,
    realtR.max_retries = max_retries,
    realtR.user_agent_rotation = user_agent_rotation
  )

  invisible(old)
}

#' Check API endpoint health
#'
#' Test connectivity to realtor.com API endpoints.
#'
#' @return A tibble with endpoint status information
#' @export
#'
#' @examples
#' \dontrun{
#' api_status()
#' }
api_status <- function() {
  endpoints <- tibble::tibble(
    name = c("listings", "geocode", "median_prices", "trends"),
    url = c(
      "https://www.realtor.com/realestateandhomes-search/",
      "https://parser-external.geo.moveaws.com/suggest",
      "https://www.realtor.com/api/v1/hulk",
      "https://www.realtor.com/myhome/trends-zip/"
    )
  )

  check_endpoint <- function(url) {
    tryCatch(
      {
        resp <- httr2::request(url) |>
          httr2::req_user_agent("realtR health check") |>
          httr2::req_timeout(10) |>
          httr2::req_error(is_error = \(resp) FALSE) |>
          httr2::req_perform()

        list(
          status = httr2::resp_status(resp),
          ok = httr2::resp_status(resp) < 400,
          latency_ms = round(httr2::resp_date(resp) |>
            difftime(Sys.time(), units = "secs") |>
            abs() * 1000)
        )
      },
      error = function(e) {
        list(status = NA_integer_, ok = FALSE, latency_ms = NA_real_)
      }
    )
  }

  results <- purrr::map(endpoints$url, check_endpoint)

  endpoints |>
    dplyr::mutate(
      status = purrr::map_int(results, "status"),
      ok = purrr::map_lgl(results, "ok"),
      latency_ms = purrr::map_dbl(results, "latency_ms"),
      checked_at = Sys.time()
    )
}
