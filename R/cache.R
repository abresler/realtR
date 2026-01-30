# Caching Infrastructure for realtR
# Uses memoise and cachem for efficient request caching

# Cache Configuration ---------------------------------------------------------

#' Get the cache directory for realtR
#' @keywords internal
#' @noRd
.get_cache_dir <- function() {
  dir <- tools::R_user_dir("realtR", "cache")
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  dir
}

#' Get the disk cache for realtR
#' @param max_age Maximum age in seconds (default: 1 hour)
#' @keywords internal
#' @noRd
.get_cache <- function(max_age = 3600) {
  cachem::cache_disk(
    dir = .get_cache_dir(),
    max_age = max_age
  )
}

#' Get an in-memory cache for session-level caching
#' @param max_age Maximum age in seconds (default: 1 hour)
#' @keywords internal
#' @noRd
.get_mem_cache <- function(max_age = 3600) {
  cachem::cache_mem(max_age = max_age)
}

# Geocode caching -------------------------------------------------------------

# Store for memoised geocode function
.memoised_geocode_impl <- NULL

#' Initialize memoised functions
#' Called from .onLoad()
#' @keywords internal
#' @noRd
.init_cache <- function() {
  # Geocode uses disk cache since addresses don't change
  # Note: dictionary functions are static tibbles, no need to memoise
  # They are already fast enough as R evaluates them instantly
}

# Memoised geocode wrapper ----------------------------------------------------

#' Create a memoised version of a geocode function
#' @param fn The function to memoise
#' @param max_age Cache max age in seconds (default: 1 week)
#' @keywords internal
#' @noRd
.memoise_geocode <- function(fn, max_age = 604800) {
  memoise::memoise(
    fn,
    cache = .get_cache(max_age = max_age)
  )
}

# Public Cache Management -----------------------------------------------------

#' Clear realtR cache
#'
#' Clears cached data including geocoding results and API responses.
#'
#' @param type Type of cache to clear: "all", "disk", or "memory" (default: "all")
#'
#' @return Invisibly returns TRUE on success
#' @export
#'
#' @examples
#' \dontrun{
#' # Clear all caches
#' clear_cache()
#'
#' # Clear only disk cache
#' clear_cache("disk")
#' }
clear_cache <- function(type = c("all", "disk", "memory")) {
  type <- match.arg(type)

  if (type %in% c("all", "disk")) {
    cache_dir <- .get_cache_dir()
    if (dir.exists(cache_dir)) {
      unlink(cache_dir, recursive = TRUE)
      dir.create(cache_dir, recursive = TRUE)
    }
  }

  if (type %in% c("all", "memory")) {
    if (!is.null(.memoised_geocode_impl)) {
      memoise::forget(.memoised_geocode_impl)
    }
  }

  invisible(TRUE)
}

#' Get cache information
#'
#' Returns information about the realtR cache including size and location.
#'
#' @return A list with cache information
#' @export
#'
#' @examples
#' \dontrun{
#' cache_info()
#' }
cache_info <- function() {
  cache_dir <- .get_cache_dir()

  files <- list.files(cache_dir, recursive = TRUE, full.names = TRUE)
  total_size <- sum(file.size(files), na.rm = TRUE)

  list(
    directory = cache_dir,
    file_count = length(files),
    total_size_bytes = total_size,
    total_size_mb = round(total_size / 1024 / 1024, 2)
  )
}
