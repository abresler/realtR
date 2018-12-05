
# new_trends --------------------------------------------------------------
.generate_zipcode_trend_urls <- 
  function(zipcodes = c(20852, 10016, 10010)) {
    urls <-
      glue::glue("https://www.realtor.com/myhome/trends-zip/{zipcodes}") %>% as.character()
    data_frame(zipcodeLocation = as.character(zipcodes), 
               urlTrendAPI = urls)
  }

.parse_zip_trend_url <- 
  function(url = "https://www.realtor.com/myhome/trends-zip/21842") {
    json_data <- 
      url %>% 
      .curl_json() %>% 
      fromJSON(simplifyDataFrame = T)
    
    
    df_trends <- json_data$trends
    df_classes <-
      df_trends %>% map(class) %>% as_data_frame() %>% gather(column, class)
    regular_cols <-
      df_classes %>% filter(!class %>% str_detect("data.frame|list")) %>% pull(column)
    data <-
      df_trends %>%
      select(regular_cols) %>%
      as_data_frame()
    
    data <-
      data %>%
      purrr::set_names(.resolve_names(data)) %>%
      mutate(idRow = 1:n())
    
    has_median <-
      df_classes %>%
      filter(column == "median") %>% nrow() > 0
    
    if (has_median) {
      df_median <-
        df_trends$median %>% as_data_frame()
      data <-
        data %>%
        left_join(
          df_median %>%
            purrr::set_names(.resolve_names(df_median) %>% str_c("Median")) %>%
            mutate(idRow = 1:n()),
          by = "idRow"
          
        )
    }
    
    has_listings <-
      df_classes %>%
      filter(column == "listing_count") %>% nrow() > 0
    
    if (has_listings) {
      df_listings <-
        df_trends$listing_count %>% as_data_frame()
      
      data <-
        data %>%
        left_join(df_listings %>%
                    purrr::set_names(.resolve_names(df_listings)) %>% mutate(idRow = 1:n()),
                  by = "idRow")
    }
    
    data <- 
      data %>% 
      dplyr::select(-dplyr::matches("idRow"))
    
    data <- 
      data %>%
      mutate(
        yearMonth = glue::glue("{yearData}-{monthData}-01") %>% lubridate::ymd(),
        daysInMonth = days_in_month(yearMonth) %>% as.integer(),
        dateData = glue::glue("{yearData}-{monthData}-{daysInMonth}") %>% lubridate::ymd(),
        urlTrendAPI = url
      ) %>%
      select(-one_of(c("yearMonth", "daysInMonth"))) %>%
      select(dateData, everything())
    
    pct_cols <- 
      data %>% select(matches("pct")) %>% names()
    
    if (length(pct_cols) > 0) {
      data <- 
        data %>% 
        mutate_at(pct_cols,
                  funs(. / 100))
    }
    
    data
    
    
    data
  }

#' Market trends for zipcodes
#'
#' @param zipcodes vector of zipcodes
#' @param return_message if \code{TRUE} returns a message
#'
#' @return \code{data_frame}
#' @export
#'
#' @examples
#' trends_zipcodes(zipcodes = c("90210", "10016"))
trends_zipcodes <-
  function(zipcodes = NULL,
           return_message = T) {
    
    if (zipcodes %>% purrr::is_null()) {
      stop("Enter a vector of zipcodes")
    }
    
    df_urls <- 
      .generate_zipcode_trend_urls(zipcodes = zipcodes)
    
    .parse_zip_trend_url_safe <- 
      purrr::possibly(.parse_zip_trend_url, data_frame())
    
    all_data <- 
      1:nrow(df_urls) %>% 
      map_df(function(x){
        df_row <- df_urls %>% dplyr::slice(x)
        url <- df_row$urlTrendAPI
        zip <- df_row$zipcodeLocation
        if (return_message) {
          glue::glue("Acquiring market trends for zipcode: {zip}") %>% cat(fill = T)
        }
        
        data <- 
          .parse_zip_trend_url_safe(url = url)
        
        data
      })
    
    all_data <- 
      all_data %>% 
      left_join(df_urls, by = "urlTrendAPI") %>% 
      dplyr::select(zipcodeLocation, everything())
    
    all_data
  }