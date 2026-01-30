# Package Initialization

.onLoad <- function(libname, pkgname) {
  # Initialize memoised cache functions
  .init_cache()
}
