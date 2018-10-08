.realtor_names  <- 
  function(data) {
    dict_names <- dictionary_realtor_names()
    actual_names <-
      names(data) %>%
      map_chr(function(x) {
        df_row <-
          dict_names %>%
          filter(nameRealtor == x)
        
        if (df_row %>% nrow() == 0) {
          glue::glue("Missing {x}") %>% message()
          return(x)
        }
        df_row$nameActual %>% unique() %>% .[[1]]
      })
    actual_names
  }

.dictionary_listing_names <- 
  function() {
    data_frame(nameRealtor = c("date", "price", "sqft", "source", "event_name", "price_changed", 
                               "price_changed_display", "datasource_name", "date_display", "price_display", 
                               "event_name_display", "price_sqft_display", "datasource_name_display", 
                               "photos", "timeline_date", "event_year", "price_difference", 
                               "price_changed_percent", "selling_agent_name", "selling_agent_url", 
                               "selling_office_name", "selling_office_url", "markup_price", 
                               "markup_price_display", "markup_percent", "markup_time", "description", 
                               "year_marker", "permit_type"),
               nameActual = c("dateListing", "priceListing", "areaPropertySF", "sourceListing", "nameListingEvent", "amountPriceListingChange", 
                              "removeChange", "nameSourceData", "removeDate", "removePriceDisplay", 
                              "remove_event_name_display", "pricePerSF", "remove_datasource_name_display", 
                              "remove_photos", "dateTimeline", "yearListing", "remove_price_difference", 
                              "pctPriceChange", "nameAgent", "urlAgent", 
                              "nameBrokerage", "urlBrokerage", "amountPriceMarkup", 
                              "remove_markup_price_display", "pctPriceMarkup", "remove_markup_time", "descriptionListing", 
                              "remove_year_marker", "typePermit")
               
    )
  }

.munge_listing_api <-
  function(json_data, property_id) {
    idProperty <- property_id
    
    json_property <- json_data[["property"]]
    
    df_prop_cols <-
      json_property %>% map(class) %>% flatten_df() %>% gather(item, value)
    
    df_base_cols <-
      df_prop_cols %>%
      filter(!value %in% c("NULL", "data.frame", "list"))
    
    data <-
      json_property[df_base_cols$item] %>% flatten_df()
    
    if (names(data)[names(data) == ""] %>% length() > 0) {
      data <- data[, !names(data) == ""]
    }
    if (data %>% tibble::has_name("state")) {
      data <-
        data %>%
        rename(nameStateProperty = state)
    }
    
    actual_names <-
      data %>% .realtor_names()
    
    data <-
      data %>%
      purrr::set_names(actual_names)
    
    if( data %>% tibble::has_name("countDaysListed")) {
      data <- data %>% 
        mutate(countDaysListed = countDaysListed %>% readr::parse_number())
    }
    
    if (data %>% tibble::has_name("hasGarage")) {
      data <- data %>% 
        mutate(hasGarage = hasGarage == "yes")
    }
    
    data <-
      data %>%
      .munge_realtor()
    
    df_cols <-
      df_prop_cols %>%
      filter(value %in% c("data.frame")) %>%
      pull(item)
    
    data_cols <-
      df_cols %>%
      map(function(column) {
        column %>% message()
        df <-
          json_property[[column]] %>% as_data_frame()
        
        if (column == "neighborhoods") {
          df <- df %>%
            purrr::set_names(
              c(
                "nameNeighborhood",
                "cityNeighborhood",
                "stateNeighborhood",
                "typeNeighborhood",
                "idRealtorNeighborhood"
              )
            ) %>%
            mutate(idProperty) %>%
            nest(-idProperty, .key = dataNeighborhood)
          return(df)
        }
        
        
        if (column == "price_history") {
          dict_names <- .dictionary_listing_names()
          actual_names <- names(df) %>%
            map_chr(function(name) {
              dict_names %>%
                filter(nameRealtor == name) %>%
                pull(nameActual)
            })
          df <- df %>%
            purrr::set_names(actual_names) %>%
            dplyr::select(-matches("remove"))
          
          df <-
            df %>%
            .munge_realtor() %>%
            mutate(idProperty) %>%
            nest(-idProperty, .key = dataListingHistory)
          return(df)
        }
        
        if (column == "photos") {
          df <-
            df %>% select(urlPhoto = url) %>%
            mutate(idProperty) %>%
            nest(-idProperty, .key = dataPhotos)
          return(df)
        }
        
        if (column == "features") {
          feature_names <- names(df)
          
          df <-
            1:length(feature_names) %>%
            map_df(function(x) {
              feature_name <-
                feature_names[[x]]
              flattened_feature <-
                df[x][[feature_name]]
              if (flattened_feature %>% purrr::is_null()) {
                return(data_frame())
              }
              
              if (class(flattened_feature) == "logical") {
                return(data_frame())
              }
              
              items <-
                flattened_feature  %>% discard(purrr::is_null) %>% flatten_chr()
              if (items %>% length() == 0) {
                return(data_frame())
              }
              data_frame(typeFeature = feature_name, nameFeature = items)
            })
          
          if (nrow(df) == 0) {
            return(data_frame())
          }
          df <- df %>%
            mutate(idProperty) %>%
            nest(-idProperty, .key = "dataFeatures")
          return(df)
        }
        
      }) %>%
      purrr::discard(purrr::is_null) %>% 
      purrr::reduce(left_join) %>%
      suppressMessages() %>% 
      suppressWarnings()
    
    data <-
      data %>%
      bind_cols(data_cols %>% select(-idProperty)) %>%
      suppressMessages()
    
    df_list_cols <-
      df_prop_cols %>%
      filter(value %in% c("list")) %>%
      filter(!item %in% c("saved_resource_note")) %>%
      pull(item)
    
    
    data_list_cols <-
      df_list_cols %>%
      map(function(column) {
        if (column == "flags") {
          df <- json_property[[column]] %>% flatten_df()
          actual_names <-
            .realtor_names(data = df)
          
          df <- df %>%
            purrr::set_names(actual_names) %>%
            mutate(idProperty) %>%
            nest(-idProperty, .key = "dataPropertyFlags")
          return(df)
        }
        
        if (column == "listing_provider") {
          df <- json_property[[column]] %>% flatten_df()
          actual_names <-
            .realtor_names(data = df)
          df <-
            df %>% purrr::set_names(actual_names) %>%
            mutate(idProperty) %>%
            nest(-idProperty, .key = "dataBrokerListing")
          return(df)
        }
        
        if (column == "building_info") {
          df <- json_property[[column]] %>% flatten_df()
          actual_names <-
            .realtor_names(data = df)
          df <-
            df %>% purrr::set_names(actual_names) %>%
            mutate(idProperty) %>%
            nest(-idProperty, .key = "dataPropertyInfo")
          return(df)
        }
        
        df <-
          json_property[[column]] %>% as_data_frame()
        
        if (nrow(df) == 0) {
          return(invisible())
        }
        
        if (column == "lead_attributes") {
          dict_names <- dictionary_realtor_names()
          actual_names <-
            names(df) %>%
            map_chr(function(name) {
              dict_names %>%
                filter(nameRealtor == name) %>%
                pull(nameActual) %>%
                .[[1]]
            })
          df <- df %>% purrr::set_names(actual_names) %>%
            mutate(idProperty) %>%
            nest(-idProperty, .key = "dataLeadAttributes")
          return(df)
        }
        
        if (column == "branding") {
          feature_names <- names(df)
          
          df <-
            1:length(feature_names) %>%
            map(function(x) {
              feature_name <-
                feature_names[[x]]
              df_feature <-
                df[x][[feature_name]] %>% flatten() %>% discard(purrr::is_null) %>% flatten_df() %>% select(-one_of("show_realtor_logo"))
              
              append_name <-
                case_when(feature_name %>% str_to_lower() == "agent" ~ "Agent",
                          TRUE ~ "Brokerage")
              
              names(df_feature) <-
                names(df_feature) %>% str_c(append_name)
              df_feature <-
                df_feature %>%
                mutate(idProperty)
              return(df_feature)
            }) %>%
            purrr::reduce(left_join) %>%
            suppressMessages()
          
          names(df) <- names(df) %>% str_replace_all("link", "url")
          df <- df %>%
            mutate(idProperty) %>%
            nest(-idProperty, .key = "dataBrokerBranding")
          return(df)
        }
        
      }) %>%
      discard(purrr::is_null) %>%
      purrr::reduce(left_join) %>%
      suppressMessages() %>%
      suppressWarnings()
    
    data <-
      data %>%
      bind_cols(data_list_cols %>% select(-idProperty)) %>%
      suppressMessages()
    
    col_order <-
      names(data)[!names(data) %>% str_detect("data")]
    
    if (data %>% tibble::has_name("dataBrokerListing")) {
      data <-
        data %>%
        select(idProperty, dataBrokerListing) %>% unnest() %>%
        left_join(data %>% select(-dataBrokerListing)) %>%
        suppressMessages()
    }
    
    if (data %>% tibble::has_name("dataPropertyFlags")) {
      data <-
        data %>%
        select(idProperty, dataPropertyFlags) %>% unnest() %>%
        left_join(data %>% select(-dataPropertyFlags)) %>%
        suppressMessages()
    }
    
    data <-
      data %>%
      select(one_of(col_order), everything())
    
    data
    
    
  }

.parse_listing_api_url <-
  function(url = "https://www.realtor.com/property-overview/M3372870765", sleep_time = NULL) {
    idProperty <-
      url %>% str_remove_all("https://www.realtor.com/property-overview/M3") %>% as.numeric()
    
    json_data <-
      .curl_json(url = url) %>%
      jsonlite::fromJSON(simplifyVector = T, simplifyDataFrame = T) 
    
    data <- 
      .munge_listing_api(json_data = json_data, property_id = idProperty) %>% suppressWarnings() %>% 
      mutate(urlPropertyAPI = url)
    
    if (!sleep_time %>% purrr::is_null()) {
      
    }
    
    data
  }

#' Parse listing urls
#'
#' Parses a vector of listing API urls
#'
#' @param urls vector of urls
#' @param return_message if \code{TRUE} returns a message - default \code{FALSE}
#' @param sleep_time sleep time in between url
#'
#' @return a \code{data_frame}
#' @export
#'
#' @examples
parse_listing_urls <-
  function(urls = NULL,
           sleep_time = 1,
           return_message = TRUE) {
    
    .parse_listing_api_url_safe <-
      purrr::possibly(.parse_listing_api_url, data_frame())
    all_data <-
      urls %>%
      map_df(function(url) {
        if (return_message) {
          glue::glue(
            "Parsing {url %>% str_replace_all('https://www.realtor.com/property-overview/', '')}"
          ) %>%
            message()
        }
        .parse_listing_api_url_safe(url = url,
                                    sleep_time = sleep_time) %>%
          suppressWarnings()
      }) %>% 
      suppressWarnings()
    all_data
  }