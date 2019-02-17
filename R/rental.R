
# estimates ---------------------------------------------------------------

.generate_rental_urls <- 
  function(zipcodes = c(20852, 10016, 10010)) {
    urls <- glue::glue("https://www.realtor.com/myhome/rental-estimate/zip/{zipcodes}") %>% as.character()
    tibble(zipcodeLocation = as.character(zipcodes), 
               urlRentalEstimate = urls)
  }

.parse_rental_estimate <- 
  function(url = "https://www.realtor.com/myhome/rental-estimate/zip/94086") {
    data <-
      url %>% jsonlite::fromJSON(simplifyDataFrame = T) %>% 
      as_tibble()
    
    actual_names <-
      .resolve_names(data = data)
    
    data %>%
      purrr::set_names(actual_names) %>%
      mutate(
        roomsEstimated = case_when(
          (rentEstimatedMonthlyModerate > 0 ) && (rentPerBedroomMonthlyModerate > 0)~ rentEstimatedMonthlyModerate / rentPerBedroomMonthlyModerate,
          TRUE ~ NA_real_),
        rentAnnualEstimatedModerate = 12 * rentEstimatedMonthlyModerate,
        dateSearch = Sys.Date(),
        urlRentalEstimate = url
      ) %>%
      select(dateSearch, everything())
    
  }

#' Moderate rental estimates
#' 
#' Returns moderate rental estimates for 
#' specified zipcodes
#'
#' @param zipcodes vector of zipcodes
#' @param return_message if \code{TRUE} returns a message
#'
#' @return
#' @export
#'
#' @examples
#' rental_estimates(zipcodes = c("20852", "10016", "90210"), return_message = T)
rental_estimates <- 
  function(zipcodes = NULL,
           return_message = T) {
    
    if (zipcodes %>% purrr::is_null()) {
      stop("Enter a vector of zipcodes")
    }
    df_urls <- 
      .generate_rental_urls(zipcodes = zipcodes)
    
    .parse_rental_estimate_safe <- 
      purrr::possibly(.parse_rental_estimate, tibble())
    
    all_data <- 
      1:nrow(df_urls) %>% 
      map_df(function(x){
        df_row <- df_urls %>% dplyr::slice(x)
        url <- df_row$urlRentalEstimate
        zip <- df_row$zipcodeLocation
        if (return_message) {
          glue::glue("Acquiring rental estimates for zipcode: {zip}") %>% cat(fill = T)
        }
        
        data <- 
          .parse_rental_estimate_safe(url = url)
        
        data
      })
    
    all_data %>% 
      left_join(df_urls, by = "urlRentalEstimate") %>% 
      mutate(dateSearch = Sys.Date()) %>% 
      select(dateSearch, zipcodeLocation, rentAnnualEstimate, everything())
  }