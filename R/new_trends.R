
# new_trends --------------------------------------------------------------
.generate_zipcode_trend_urls <- 
  function(zipcodes = c(20852, 10016, 10010)) {
    urls <-
      glue("https://www.realtor.com/myhome/trends-zip/{zipcodes}") %>% as.character()
    tibble(zipcodeLocation = as.character(zipcodes), 
               urlTrendAPI = urls)
  }

.parse_zip_trend_url <- 
  function(url = "https://www.realtor.com/myhome/trends-zip/10016") {
    json_data <-
      url %>%
      .curl_json() %>%
      fromJSON(simplifyDataFrame = T)
    
    
    df_trends <- json_data$trends
    
    df_classes <-
      df_trends %>% map(class) %>% as_tibble() %>% gather(column, class)
    
    regular_cols <-
      df_classes %>% filter(!class %>% str_detect("data.frame|list")) %>% pull(column)
    
    data <-
      df_trends %>%
      select(regular_cols) %>%
      as_tibble()
    
    data <-
      data %>%
      set_names(.resolve_names(data)) %>%
      mutate(idRow = 1:n())
    
    has_median <-
      df_classes %>%
      filter(column == "median") %>% nrow() > 0
    
    if (has_median) {
      df_median <-
        df_trends$median %>% as_tibble()
      
      d <- data.frame(df_median$by_prop_type) %>% as_tibble()
      
      d <- 
        d$condo_townhome_rowhome_coop %>%
        setNames(c(
          "amountClosingPriceTownHouseCondo",
          "amountListingPriceTownHouseCondo"
        )) %>%
        bind_cols(d$single_family %>%
                    setNames(
                      c(
                        "amountClosingPriceSingleFamily",
                        "amountListingPriceSingleFamily"
                      )
                    )) %>%
        bind_cols(d$multi_family %>%
                    setNames(
                      c(
                        "amountClosingPriceMultifamily",
                        "amountListingPriceMultifamily"
                      )
                    )) %>% 
        as_tibble()
      
      df_median <- 
        df_median %>% 
        select(-one_of("by_prop_type")) %>% as_tibble() %>% 
        bind_cols(d)
      
      data <-
        data %>%
        left_join(
          df_median %>%
            set_names(.resolve_names(df_median) %>% str_c("Median")) %>%
            mutate(idRow = 1:n()),
          by = "idRow"
          
        )
    }
    
    has_listings <-
      df_classes %>%
      filter(column == "listing_count") %>% nrow() > 0
    
    if (has_listings) {
      df_listings <-
        df_trends$listing_count %>% as_tibble()
      
      data <-
        data %>%
        left_join(df_listings %>%
                    set_names(.resolve_names(df_listings)) %>% mutate(idRow = 1:n()),
                  by = "idRow")
    }
    
    data <- 
      data %>% 
      dplyr::select(-dplyr::matches("idRow"))
    
    data <- 
      data %>%
      mutate(
        yearMonth = glue("{yearData}-{monthData}-01") %>% lubridate::ymd(),
        daysInMonth = days_in_month(yearMonth) %>% as.integer(),
        dateData = glue("{yearData}-{monthData}-{daysInMonth}") %>% lubridate::ymd(),
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
    
    data <- data %>% 
      remove_na()
    
    data
  }

#' Market trends for zipcodes
#'
#' @param zipcodes vector of zipcodes
#' @param return_message if \code{TRUE} returns a message
#'
#' @return \code{tibble}
#' @export
#'
#' @examples
#' trends_zipcodes(zipcodes = c("90210", "10016"))
trends_zipcodes <-
  function(zipcodes = NULL,
           return_message = T) {
    
    if (zipcodes %>% is_null()) {
      stop("Enter a vector of zipcodes")
    }
    
    df_urls <- 
      .generate_zipcode_trend_urls(zipcodes = zipcodes)
    
    .parse_zip_trend_url_safe <- 
      possibly(.parse_zip_trend_url, tibble())
    
    all_data <- 
      1:nrow(df_urls) %>% 
      map_df(function(x){
        df_row <- df_urls %>% dplyr::slice(x)
        url <- df_row$urlTrendAPI
        zip <- df_row$zipcodeLocation
        if (return_message) {
          glue("Acquiring market trends for zipcode: {zip}") %>% cat(fill = T)
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