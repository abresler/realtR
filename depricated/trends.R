# market_trends -----------------------------------------------------------
.generate_market_trend_url <-
  function(location_name = c("Bethesda, MD")) {
    has_comma <- location_name %>% str_detect("\\,")
    if (has_comma) {
      is_city_state <- location_name %>% str_count("\\,") == 1
      if (is_city_state) {
        df_loc_slug <-
          location_name %>%
          str_trim() %>%
          parse_location()
        
        city <-
          df_loc_slug$citySearch %>%
          str_replace_all("\\ ", "\\-")
        state <- df_loc_slug$stateSearch
        
        location_slug <-
          glue("{city}_{state}") %>% as.character()
        
        if (state %>% nchar() > 4) {
          stop("Has to be the 2 digit state slug buddy")
        }
        
        url <-
          glue::glue("https://www.realtor.com/local/markettrends/city/{location_slug}") %>%
          as.character()
        
        data <-
          df_loc_slug %>%
          mutate(locationSearch = location_name) %>%
          mutate(urlAPI = url) %>%
          select(locationSearch, everything())
        
        
        data <-
          df_loc_slug %>%
          mutate(urlAPI = url) %>%
          select(locationSearch, everything())
      } else {
        df_loc_slug <-
          location_name %>%
          str_trim() %>%
          parse_location()
        
        city <-
          df_loc_slug$citySearch %>%
          str_replace_all("\\ ", "\\-")
        
        location_slug <-
          glue("{city}_{df_loc_slug$stateSearch}") %>% as.character()
        
        state <- df_loc_slug$stateSearch
        
        if (state %>% nchar() > 4) {
          stop("Has to be the 2 digit state slug buddy")
        }
        
        url <-
          glue::glue("https://www.realtor.com/local/markettrends/city/{location_slug}") %>%
          as.character()
        
        data <-
          df_loc_slug %>%
          mutate(locationSearch = location_name) %>%
          mutate(urlAPI = url) %>%
          select(locationSearch, everything())
        
        
        data <-
          df_loc_slug %>%
          mutate(urlAPI = url) %>%
          select(locationSearch, everything())
      }
    } else {
      data <-
        parse_location(location_name = location_name)
      url <-
        glue::glue("https://www.realtor.com/local/markettrends/city/{location_name}") %>%
        as.character()
      
      data <-
        data %>%
        mutate(urlAPI = url)
    }
    data
  }

.generate_market_trend_urls <-
  function(locations = c("Bethesda, MD")) {
    .generate_market_trend_url_safe <-
      purrr::possibly(.generate_market_trend_url, data_frame())
    
    locations %>%
      future_map_dfr(function(location_name) {
        .generate_market_trend_url_safe(location_name = location_name)
      }) %>%
      dplyr::select(one_of(
        c(
          "locationSearch",
          "citySearch",
          "stateSearch",
          "zipcodeSearch",
          "urlAPI"
        )
      ), everything()) %>%
      suppressWarnings() %>%
      suppressMessages()
  }


.parse_market_trend_url <-
  function(url = "https://www.realtor.com/local/markettrends/city/Marietta_GA") {
    json_data <-
      .curl_json(url = url) %>%
      jsonlite::fromJSON(flatten = T, simplifyDataFrame = T)
    
    df_geo <-
      json_data$geo %>% flatten_df()
    
    df_names <- dictionary_realtor_names()
    actual_names <-
      names(df_geo) %>%
      map_chr(function(name) {
        df_row <- df_names %>% filter(nameRealtor == name)
        if (df_row %>% nrow() == 0) {
          glue::glue("Missing {name}") %>%
            cat(fill = T)
          return(name)
        }
        df_row %>% pull(nameActual)
      }) %>%
      str_replace_all("Property", "Search")
    
    df_geo <-
      df_geo %>%
      purrr::set_names(actual_names) %>%
      mutate(urlAPI = url)
    
    data <-
      json_data$trends %>%
      as_data_frame()
    
    actual_names <-
      names(data) %>%
      map_chr(function(name) {
        df_row <- df_names %>% filter(nameRealtor == name)
        if (df_row %>% nrow() == 0) {
          glue::glue("Missing {name}") %>%
            cat(fill = T)
          return(name)
        }
        df_row %>% pull(nameActual)
      })
    
    data <-
      data %>%
      purrr::set_names(actual_names) %>%
      remove_na() %>%
      mutate(
        dateData = glue::glue("{yearData}-{monthData}-{01}") %>% as.character() %>%
          lubridate::ymd()
      ) %>%
      select(dateData, everything())
    
    data <-
      data %>%
      mutate(dateData = dateData %m+% months(1) - 1)
    
    data <-
      data %>%
      mutate_at(data %>% select(matches("pct")) %>% names(),
                funs(. / 100)) %>%
      .munge_realtor()
    
    if (data %>% tibble::has_name("priceListingMedian")) {
      data <-
        data %>%
        mutate(
          areaPropertySFMedian = (priceListingMedian / pricePerSFMedian) %>% round(digits = 2)
        )
    }
    
    if (data %>% tibble::has_name("priceRentMedian")) {
      data <- 
        data %>% 
        mutate(pctRentYield = (priceRentMedian * 12) / priceListingMedian)
    }
    
    data <- 
      data %>% 
      mutate(urlAPI = url)  
    
    data <-
      data %>%
      left_join(df_geo) %>%
      dplyr::select(one_of(c(
        "stateSearch", "citySearch", "zipcodeSearch"
      )), everything()) %>%
      suppressMessages() %>%
      suppressWarnings()
    data
    
    
  }


.parse_market_trend_urls <-
  function(urls = "https://www.realtor.com/local/markettrends/city/Marietta_GA",
           return_message = TRUE) {
    .parse_market_trend_url_safe <-
      purrr::possibly(.parse_market_trend_url, data_frame())
    urls %>%
      future_map_dfr(function(url) {
        if (return_message) {
          glue::glue("Parsing {url %>% str_replace_all('https://www.realtor.com/', '')}") %>%
            cat(fill = T)
        }
        .parse_market_trend_url_safe(url = url)
      })
    
  }

#' Market trends
#'
#' This function returns market
#' trend information dating back to 2015
#' for the user's specified locations
#'
#' @param locations vector of location names, location name must contain
#' a city name and a comma ie "Brooklyn, NY" or a zipcode
#' @param return_wide if \code{TRUE} returns data in wide form
#' @param return_message if \code{TRUE} returns a message
#' @param ... extra parameters
#'
#' @return a \code{data_frame}
#' @export
#'
#' @examples
#' trends(locations = c("Greenwich, CT", "New London, CT", "Woodside, CA", 90210), return_message = F, return_wide = T)
trends <-
  function(locations = NULL,
           return_wide = TRUE,
           return_message = T,
           ...) {
    if (locations %>% purrr::is_null()) {
      stop("Please enter locations")
    }
    
    if (locations %>% purrr::is_null()) {
      stop("Please enter location names!!")
    }
    
    .generate_market_trend_urls_safe <-
      purrr::possibly(.generate_market_trend_urls, data_frame())
    
    df_urls <-
      .generate_market_trend_urls(locations = as.character(locations))
    
    if (df_urls %>% nrow() == 0) {
      "No results" %>% cat(fill = T)
      return(invisible())
    }
    
    .parse_market_trend_urls_safe <-
      purrr::possibly(.parse_market_trend_urls, data_frame())
    
    all_data <-
      .parse_market_trend_urls(urls = df_urls$urlAPI, return_message = return_message)
    
    all_data <-
      all_data %>%
      select(-one_of(c(
        "stateSearch", "citySearch", "zipcodeSearch"
      ))) %>%
      left_join(df_urls) %>%
      dplyr::select(
        one_of(
          "dateData",
          "locationSearch",
          "streetOrNeighborhoodSearch",
          "stateSearch",
          "citySearch",
          "zipcodeSearch"
        ),
        everything()
      ) %>%
      arrange(dateData) %>%
      suppressMessages() %>%
      suppressWarnings()
    
    if (all_data %>% nrow() == 0) {
      "No results" %>% cat(fill = T)
      return(invisible())
    }
    
    if (!return_wide) {
      gather_cols <-
        c(
          "stateSearch",
          "citySearch",
          "zipcodeSearch",
          "dateData",
          "yearData",
          "monthData",
          "temperatureMarket",
          "typeSalesToInventoryFavor",
          "urlAPI"
        )
      
      gather_cols <- gather_cols[gather_cols %in% names(all_data)]
      all_data <-
        all_data %>%
        gather(metric, value, -gather_cols, na.rm = T)
    }
    
    if (locations %>% str_to_lower() %>% str_detect("county") %>% sum(na.rm = T) >= 1) {
      if (all_data %>% tibble::has_name("citySearch")) {
        all_data <- 
          all_data %>% 
          rename(cityCountySearch = citySearch)
      }
    }
    
    all_data %>%
      remove_columns()
    
  }
