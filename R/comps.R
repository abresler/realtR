# Comparable Sales (Comps) Analysis
# Functions for finding and analyzing comparable sales

#' Find comparable sales for property valuation
#'
#' Retrieves recently sold properties similar to a target property
#' for use in property valuation and pricing analysis. This is the
#' core function for comparable sales analysis.
#'
#' @param address Character. Target property address (e.g., "123 Main St, Boston MA").
#'   Can also be a zip code or city name.
#' @param radius_miles Numeric. Search radius in miles (default: 0.5).
#' @param max_age_months Numeric. Only include sales from last N months (default: 6).
#' @param match_beds Logical. Match on bedroom count within tolerance (default: TRUE).
#' @param match_baths Logical. Match on bathroom count within tolerance (default: TRUE).
#' @param match_sqft Logical. Match on square footage within tolerance (default: TRUE).
#' @param match_type Logical. Match on property type (default: TRUE).
#' @param bed_tolerance Integer. Bedroom match tolerance, +/- (default: 1).
#' @param bath_tolerance Numeric. Bathroom match tolerance, +/- (default: 1).
#' @param sqft_tolerance Numeric. Square footage match tolerance as proportion (default: 0.20 = 20%).
#' @param max_results Integer. Maximum number of comps to return (default: 20).
#' @param include_target Logical. Include target property info in output (default: TRUE).
#' @param return_message Logical. Print progress messages (default: TRUE).
#'
#' @return A tibble with comparable sales including:
#' \itemize{
#'   \item \code{addressProperty} - Property address
#'   \item \code{priceSold} - Sale price
#'   \item \code{dateSold} - Sale date
#'   \item \code{pricePerSF} - Price per square foot
#'   \item \code{countBeds} - Number of bedrooms
#'   \item \code{countBaths} - Number of bathrooms
#'   \item \code{areaPropertySF} - Square footage
#'   \item \code{typeProperty} - Property type
#'   \item \code{similarityScore} - Similarity to target (0-100)
#'   \item \code{distanceMiles} - Distance from target in miles
#' }
#'
#' @export
#' @family valuation
#' @family listing search
#'
#' @examples
#' \dontrun{
#' # Find comps for an address
#' comps_data <- comps("4915 Greenway Dr, Bethesda, MD 20816")
#'
#' # Wider search radius, more recent sales only
#' comps_data <- comps(
#'   address = "4915 Greenway Dr, Bethesda, MD 20816",
#'   radius_miles = 1,
#'   max_age_months = 3
#' )
#'
#' # Strict matching on property characteristics
#' comps_data <- comps(
#'   address = "123 Main St, Boston MA",
#'   bed_tolerance = 0,  # Exact bedroom match
#'   sqft_tolerance = 0.10  # Within 10% square footage
#' )
#' }
comps <- function(address,
                  radius_miles = 0.5,
                  max_age_months = 6,
                  match_beds = TRUE,
                  match_baths = TRUE,
                  match_sqft = TRUE,
                  match_type = TRUE,
                  bed_tolerance = 1L,
                  bath_tolerance = 1,
                  sqft_tolerance = 0.20,
                  max_results = 20L,
                  include_target = TRUE,
                  return_message = TRUE) {
  # Validate inputs
  if (missing(address) || is.null(address) || address == "") {
    rlang::abort("Address is required for comparable sales search.")
  }

  if (return_message) {
    message(glue::glue("Finding comparable sales for: {address}"))
  }

  # Step 1: Geocode the target address
  target_geo <- tryCatch(
    {
      geocode(location = address)
    },
    error = function(e) {
      rlang::abort(glue::glue("Could not geocode address: {address}"))
    }
  )

  if (nrow(target_geo) == 0) {
    rlang::abort(glue::glue("Could not geocode address: {address}"))
  }

  target_lat <- target_geo$latitudeLocation[1]
  target_lon <- target_geo$longitudeLocation[1]
  target_location <- target_geo$slugLocation[1]

  if (return_message) {
    message(glue::glue("  Target location: {target_lat}, {target_lon}"))
  }

  # Step 2: Try to get target property details from active or recent listings
  target_info <- .get_target_property_info(address, return_message)

  # Step 3: Get sold listings in the area
  if (return_message) {
    message(glue::glue("  Searching for sold properties within {radius_miles} miles..."))
  }

  sold_listings <- .get_sold_listings(
    location = target_location,
    radius_miles = radius_miles,
    max_age_months = max_age_months,
    return_message = return_message
  )

  if (is.null(sold_listings) || nrow(sold_listings) == 0) {
    if (return_message) {
      message("  No sold listings found in the area.")
    }
    return(tibble::tibble())
  }

  if (return_message) {
    message(glue::glue("  Found {nrow(sold_listings)} sold properties"))
  }

  # Step 4: Calculate distance from target for all sold properties
  sold_listings <- sold_listings %>%
    dplyr::mutate(
      distanceMiles = .haversine_distance(
        target_lat, target_lon,
        latitudeProperty, longitudeProperty
      )
    ) %>%
    dplyr::filter(distanceMiles <= radius_miles)

  if (nrow(sold_listings) == 0) {
    if (return_message) {
      message("  No properties within search radius.")
    }
    return(tibble::tibble())
  }

  # Step 5: Filter by matching criteria if target info available
  if (!is.null(target_info) && nrow(target_info) > 0) {
    sold_listings <- .filter_comps(
      sold_listings,
      target_info,
      match_beds = match_beds,
      match_baths = match_baths,
      match_sqft = match_sqft,
      match_type = match_type,
      bed_tolerance = bed_tolerance,
      bath_tolerance = bath_tolerance,
      sqft_tolerance = sqft_tolerance
    )

    # Step 6: Calculate similarity scores
    sold_listings <- .calculate_similarity(sold_listings, target_info)
  } else {
    # No target info, assign neutral similarity scores
    sold_listings <- sold_listings %>%
      dplyr::mutate(similarityScore = 50)
  }

  # Step 7: Sort by similarity and limit results
  result <- sold_listings %>%
    dplyr::arrange(dplyr::desc(similarityScore), distanceMiles) %>%
    dplyr::slice_head(n = max_results)

  # Add target info as attribute if requested

  if (include_target && !is.null(target_info) && nrow(target_info) > 0) {
    attr(result, "target") <- target_info
  }

  if (return_message) {
    message(glue::glue("  Returning {nrow(result)} comparable sales"))
  }

  result
}

# Helper Functions ------------------------------------------------------------

#' Get target property information
#' @keywords internal
#' @noRd
.get_target_property_info <- function(address, return_message = TRUE) {
  if (return_message) {
    message("  Looking up target property details...")
  }

  # Try to find the property in listings
  tryCatch(
    {
      # Try to parse address for location
      location <- address %>%
        stringr::str_extract("[0-9]{5}") # Extract zip code if present

      if (is.na(location)) {
        # Use full address
        location <- address
      }

      # Get listings and try to match
      listings_data <- listings(location = location, return_message = FALSE)

      if (nrow(listings_data) == 0) {
        return(NULL)
      }

      # Try to match by address
      address_clean <- address %>%
        stringr::str_to_upper() %>%
        stringr::str_replace_all("[^A-Z0-9 ]", "")

      matched <- listings_data %>%
        dplyr::mutate(
          address_clean = addressProperty %>%
            stringr::str_to_upper() %>%
            stringr::str_replace_all("[^A-Z0-9 ]", "")
        ) %>%
        dplyr::filter(stringr::str_detect(address_clean, stringr::str_sub(address_clean, 1, 10)[1]))

      if (nrow(matched) > 0) {
        return(matched %>% dplyr::slice(1))
      }

      # If no match, return first listing as proxy (user should verify)
      return(NULL)
    },
    error = function(e) {
      if (return_message) {
        message("  Could not retrieve target property details. Using area averages.")
      }
      return(NULL)
    }
  )
}

#' Get sold listings from realtor.com
#' @keywords internal
#' @noRd
.get_sold_listings <- function(location,
                               radius_miles = 0.5,
                               max_age_months = 6,
                               return_message = TRUE) {
  # Build URL for sold listings
  # realtor.com uses /show-recently-sold suffix
  location_slug <- location %>%
    stringr::str_replace_all(" ", "-") %>%
    stringr::str_replace_all(",", "") %>%
    stringr::str_replace_all("_", "-")

  base_url <- glue::glue(
    "https://www.realtor.com/realestateandhomes-search/{location_slug}/show-recently-sold"
  )

  tryCatch(
    {
      page <- .fetch_page(base_url)

      # Parse results from page
      .parse_sold_page(page, max_age_months = max_age_months)
    },
    error = function(e) {
      if (return_message) {
        message(glue::glue("  Error fetching sold listings: {e$message}"))
      }
      return(NULL)
    }
  )
}

#' Parse sold listings page
#' @keywords internal
#' @noRd
.parse_sold_page <- function(page, max_age_months = 6) {
  # Extract JSON data from page (realtor.com embeds data in script tags)
  scripts <- page %>%
    rvest::html_nodes("script") %>%
    rvest::html_text()

  # Find script with property data
  data_script <- scripts[stringr::str_detect(scripts, "propertySearchResults|searchResults")]

  if (length(data_script) == 0) {
    # Try alternative parsing via HTML
    return(.parse_sold_page_html(page, max_age_months))
  }

  # Extract JSON
  json_match <- stringr::str_extract(data_script[1], "\\{.+\\}")

  if (is.na(json_match)) {
    return(.parse_sold_page_html(page, max_age_months))
  }

  tryCatch(
    {
      data <- jsonlite::fromJSON(json_match, simplifyVector = TRUE, flatten = TRUE)

      # Navigate to results (structure varies)
      results <- NULL
      if (!is.null(data$props$pageProps$searchResults$results)) {
        results <- data$props$pageProps$searchResults$results
      } else if (!is.null(data$searchResults$results)) {
        results <- data$searchResults$results
      }

      if (is.null(results) || length(results) == 0) {
        return(.parse_sold_page_html(page, max_age_months))
      }

      # Convert to tibble and standardize
      .standardize_sold_results(results, max_age_months)
    },
    error = function(e) {
      .parse_sold_page_html(page, max_age_months)
    }
  )
}

#' Parse sold page via HTML scraping
#' @keywords internal
#' @noRd
.parse_sold_page_html <- function(page, max_age_months = 6) {
  # Fallback HTML parsing
  result_nodes <- page %>%
    rvest::html_nodes("[data-testid='property-card'], .property-card, .srp-item")

  if (length(result_nodes) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(result_nodes, function(node) {
    tryCatch(
      {
        tibble::tibble(
          addressProperty = node %>%
            rvest::html_node("[data-testid='card-address'], .address") %>%
            rvest::html_text() %>%
            stringr::str_trim(),
          priceSold = node %>%
            rvest::html_node("[data-testid='card-price'], .price") %>%
            rvest::html_text() %>%
            readr::parse_number(),
          countBeds = node %>%
            rvest::html_node("[data-testid='property-meta-beds'], .beds") %>%
            rvest::html_text() %>%
            readr::parse_number(),
          countBaths = node %>%
            rvest::html_node("[data-testid='property-meta-baths'], .baths") %>%
            rvest::html_text() %>%
            readr::parse_number(),
          areaPropertySF = node %>%
            rvest::html_node("[data-testid='property-meta-sqft'], .sqft") %>%
            rvest::html_text() %>%
            readr::parse_number(),
          latitudeProperty = NA_real_,
          longitudeProperty = NA_real_,
          typeProperty = NA_character_,
          dateSold = NA
        )
      },
      error = function(e) {
        tibble::tibble()
      }
    )
  }) %>%
    dplyr::filter(!is.na(addressProperty))
}

#' Standardize sold results from JSON
#' @keywords internal
#' @noRd
.standardize_sold_results <- function(results, max_age_months = 6) {
  # Map common fields
  tibble::tibble(
    addressProperty = results$location$address$line %||% NA_character_,
    cityProperty = results$location$address$city %||% NA_character_,
    stateProperty = results$location$address$state_code %||% NA_character_,
    zipcodeProperty = results$location$address$postal_code %||% NA_character_,
    priceSold = results$description$sold_price %||% results$list_price %||% NA_real_,
    dateSold = results$description$sold_date %||% NA_character_,
    countBeds = results$description$beds %||% NA_integer_,
    countBaths = results$description$baths %||% NA_real_,
    areaPropertySF = results$description$sqft %||% NA_real_,
    typeProperty = results$description$type %||% NA_character_,
    latitudeProperty = results$location$address$coordinate$lat %||% NA_real_,
    longitudeProperty = results$location$address$coordinate$lon %||% NA_real_,
    urlListing = results$href %||% NA_character_
  ) %>%
    dplyr::mutate(
      pricePerSF = dplyr::case_when(
        areaPropertySF > 0 ~ round(priceSold / areaPropertySF, 2),
        TRUE ~ NA_real_
      ),
      dateSold = lubridate::ymd(dateSold)
    ) %>%
    # Filter by date
    dplyr::filter(
      is.na(dateSold) |
        dateSold >= (Sys.Date() - lubridate::months(max_age_months))
    )
}

#' Filter comps by matching criteria
#' @keywords internal
#' @noRd
.filter_comps <- function(sold,
                          target,
                          match_beds = TRUE,
                          match_baths = TRUE,
                          match_sqft = TRUE,
                          match_type = TRUE,
                          bed_tolerance = 1,
                          bath_tolerance = 1,
                          sqft_tolerance = 0.20) {
  result <- sold

  if (match_beds && "countBeds" %in% names(target) && !is.na(target$countBeds[1])) {
    target_beds <- target$countBeds[1]
    result <- result %>%
      dplyr::filter(
        is.na(countBeds) |
          (countBeds >= target_beds - bed_tolerance &
            countBeds <= target_beds + bed_tolerance)
      )
  }

  if (match_baths && "countBaths" %in% names(target) && !is.na(target$countBaths[1])) {
    target_baths <- target$countBaths[1]
    result <- result %>%
      dplyr::filter(
        is.na(countBaths) |
          (countBaths >= target_baths - bath_tolerance &
            countBaths <= target_baths + bath_tolerance)
      )
  }

  if (match_sqft && "areaPropertySF" %in% names(target) && !is.na(target$areaPropertySF[1])) {
    target_sqft <- target$areaPropertySF[1]
    min_sqft <- target_sqft * (1 - sqft_tolerance)
    max_sqft <- target_sqft * (1 + sqft_tolerance)
    result <- result %>%
      dplyr::filter(
        is.na(areaPropertySF) |
          (areaPropertySF >= min_sqft & areaPropertySF <= max_sqft)
      )
  }

  if (match_type && "typeProperty" %in% names(target) && !is.na(target$typeProperty[1])) {
    target_type <- target$typeProperty[1] %>% stringr::str_to_lower()
    result <- result %>%
      dplyr::filter(
        is.na(typeProperty) |
          stringr::str_to_lower(typeProperty) == target_type
      )
  }

  result
}

#' Calculate similarity scores
#' @keywords internal
#' @noRd
.calculate_similarity <- function(sold, target) {
  if (nrow(sold) == 0) {
    return(sold)
  }

  sold %>%
    dplyr::mutate(
      # Bed score (25 points max)
      .bed_score = dplyr::case_when(
        is.na(countBeds) | is.na(target$countBeds[1]) ~ 12.5,
        countBeds == target$countBeds[1] ~ 25,
        abs(countBeds - target$countBeds[1]) == 1 ~ 15,
        TRUE ~ 5
      ),
      # Bath score (20 points max)
      .bath_score = dplyr::case_when(
        is.na(countBaths) | is.na(target$countBaths[1]) ~ 10,
        countBaths == target$countBaths[1] ~ 20,
        abs(countBaths - target$countBaths[1]) <= 0.5 ~ 15,
        abs(countBaths - target$countBaths[1]) <= 1 ~ 10,
        TRUE ~ 5
      ),
      # Sqft score (40 points max)
      .sqft_diff = dplyr::case_when(
        is.na(areaPropertySF) | is.na(target$areaPropertySF[1]) ~ 0.5,
        target$areaPropertySF[1] > 0 ~ abs(areaPropertySF - target$areaPropertySF[1]) / target$areaPropertySF[1],
        TRUE ~ 0.5
      ),
      .sqft_score = dplyr::case_when(
        .sqft_diff <= 0.05 ~ 40,
        .sqft_diff <= 0.10 ~ 35,
        .sqft_diff <= 0.15 ~ 30,
        .sqft_diff <= 0.20 ~ 25,
        .sqft_diff <= 0.30 ~ 15,
        TRUE ~ 5
      ),
      # Distance score (15 points max)
      .dist_score = dplyr::case_when(
        distanceMiles <= 0.1 ~ 15,
        distanceMiles <= 0.25 ~ 12,
        distanceMiles <= 0.5 ~ 10,
        distanceMiles <= 1.0 ~ 7,
        TRUE ~ 3
      ),
      # Total similarity
      similarityScore = round(.bed_score + .bath_score + .sqft_score + .dist_score)
    ) %>%
    dplyr::select(-dplyr::starts_with("."))
}

#' Haversine distance calculation
#' @keywords internal
#' @noRd
.haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Radius of Earth in miles
  R <- 3959

  # Handle vectorized inputs
  lat1_rad <- lat1 * pi / 180

lat2_rad <- lat2 * pi / 180
  lon1_rad <- lon1 * pi / 180
  lon2_rad <- lon2 * pi / 180

  dlat <- lat2_rad - lat1_rad
  dlon <- lon2_rad - lon1_rad

  a <- sin(dlat / 2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))

  R * c
}

#' Null-coalescing operator
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

# Comp Analysis Functions -----------------------------------------------------

#' Summarize comparable sales
#'
#' Provides summary statistics for a set of comparable sales.
#'
#' @param comps_data Tibble from \code{comps()} function
#'
#' @return A list with summary statistics including median, mean, and range
#' @export
#' @family valuation
#'
#' @examples
#' \dontrun{
#' comps_data <- comps("123 Main St, Boston MA")
#' summary_comps(comps_data)
#' }
summary_comps <- function(comps_data) {
  if (nrow(comps_data) == 0) {
    return(list(
      n_comps = 0,
      message = "No comparable sales to summarize"
    ))
  }

  list(
    n_comps = nrow(comps_data),
    price = list(
      median = stats::median(comps_data$priceSold, na.rm = TRUE),
      mean = mean(comps_data$priceSold, na.rm = TRUE),
      min = min(comps_data$priceSold, na.rm = TRUE),
      max = max(comps_data$priceSold, na.rm = TRUE),
      sd = stats::sd(comps_data$priceSold, na.rm = TRUE)
    ),
    price_per_sf = list(
      median = stats::median(comps_data$pricePerSF, na.rm = TRUE),
      mean = mean(comps_data$pricePerSF, na.rm = TRUE),
      min = min(comps_data$pricePerSF, na.rm = TRUE),
      max = max(comps_data$pricePerSF, na.rm = TRUE)
    ),
    sqft = list(
      median = stats::median(comps_data$areaPropertySF, na.rm = TRUE),
      mean = mean(comps_data$areaPropertySF, na.rm = TRUE)
    ),
    avg_similarity_score = mean(comps_data$similarityScore, na.rm = TRUE),
    avg_distance_miles = mean(comps_data$distanceMiles, na.rm = TRUE),
    date_range = list(
      earliest = min(comps_data$dateSold, na.rm = TRUE),
      latest = max(comps_data$dateSold, na.rm = TRUE)
    ),
    target = attr(comps_data, "target")
  )
}

#' Estimate property value from comps
#'
#' Estimates a property's market value based on comparable sales.
#' Uses similarity-weighted average of comp prices.
#'
#' @param comps_data Tibble from \code{comps()} function
#' @param method Estimation method: "weighted" (default), "median", or "mean"
#'
#' @return A list with estimated value and confidence metrics
#' @export
#' @family valuation
#'
#' @examples
#' \dontrun{
#' comps_data <- comps("123 Main St, Boston MA")
#' estimate_value(comps_data)
#' }
estimate_value <- function(comps_data, method = c("weighted", "median", "mean")) {
  method <- match.arg(method)

  if (nrow(comps_data) == 0) {
    return(list(
      estimated_value = NA_real_,
      confidence = "none",
      message = "No comparable sales for estimation"
    ))
  }

  target <- attr(comps_data, "target")

  # Calculate estimated value
  estimate <- switch(method,
    weighted = {
      weights <- comps_data$similarityScore / sum(comps_data$similarityScore, na.rm = TRUE)
      sum(comps_data$priceSold * weights, na.rm = TRUE)
    },
    median = stats::median(comps_data$priceSold, na.rm = TRUE),
    mean = mean(comps_data$priceSold, na.rm = TRUE)
  )

  # Calculate confidence based on number and quality of comps
  n_comps <- nrow(comps_data)
  avg_similarity <- mean(comps_data$similarityScore, na.rm = TRUE)

  confidence <- dplyr::case_when(
    n_comps >= 5 && avg_similarity >= 70 ~ "high",
    n_comps >= 3 && avg_similarity >= 50 ~ "medium",
    n_comps >= 1 ~ "low",
    TRUE ~ "none"
  )

  # Calculate range
  price_sd <- stats::sd(comps_data$priceSold, na.rm = TRUE)

  list(
    estimated_value = round(estimate),
    confidence = confidence,
    method = method,
    n_comps = n_comps,
    avg_similarity_score = round(avg_similarity, 1),
    value_range = list(
      low = round(estimate - price_sd),
      high = round(estimate + price_sd)
    ),
    price_per_sf_estimate = if (!is.null(target) && !is.na(target$areaPropertySF[1])) {
      round(estimate / target$areaPropertySF[1], 2)
    } else {
      NA_real_
    }
  )
}
