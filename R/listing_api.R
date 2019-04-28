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
          glue::glue("Missing {x}") %>% cat(fill = T)
          return(x)
        }
        df_row$nameActual %>% unique() %>% .[[1]]
      })
    actual_names
  }

.dictionary_listing_names <- 
  function() {
    tibble(nameRealtor = c("date", "price", "sqft", "source", "event_name", "price_changed", 
                               "price_changed_display", "datasource_name", "date_display", "price_display", 
                               "event_name_display", "price_sqft_display", "datasource_name_display", 
                               "photos", "timeline_date", "event_year", "price_difference", 
                               "price_changed_percent", "selling_agent_name", "selling_agent_url", 
                               "selling_office_name", "selling_office_url", "markup_price", 
                               "markup_price_display", "markup_percent", "markup_time", "description", 
                               "year_marker", "permit_type", "street_direction",
                               "id", "rental_estimate"),
               nameActual = c("dateListing", "priceListing", "areaPropertySF", "sourceListing", "nameListingEvent", "amountPriceListingChange", 
                              "removeChange", "nameSourceData", "removeDate", "removePriceDisplay", 
                              "remove_event_name_display", "pricePerSF", "remove_datasource_name_display", 
                              "remove_photos", "dateTimeline", "yearListing", "remove_price_difference", 
                              "pctPriceChange", "nameAgent", "urlAgent", 
                              "nameBrokerage", "urlBrokerage", "amountPriceMarkup", 
                              "remove_markup_price_display", "pctPriceMarkup", "remove_markup_time", "descriptionListing", 
                              "remove_year_marker", "typePermit",
                              "directionStreet",
                              "id", "amountRentEstimated")
               
    )
  }

.munge_listing_api <-
  function(json_data, property_id) {
    idProperty <- property_id
    
    json_property <- 
      json_data[["property"]]
    
    df_prop_cols <-
      json_property %>% future_map(class) %>% flatten_df() %>% gather(item, value)
    
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
    
    sel_col <- 
      tibble(column = names(data)) %>% mutate(idColumn = 1:n()) %>% 
      group_by(column) %>% 
      filter(idColumn == min(idColumn)) %>% 
      ungroup() %>% 
      arrange(idColumn) %>% 
      pull(idColumn)
    
    data <- data[,sel_col]
    
    if(data %>% tibble::has_name("countDaysListed")) {
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
        column %>% cat(fill = T)
        df <-
          json_property[[column]] %>% as_tibble()
        
        if (column == "lease_details") {
          df <- 
            df %>% 
            purrr::set_names(df %>% .resolve_names()) %>% 
            .munge_realtor()
          
          df <- df %>% 
            mutate(idProperty) %>% 
            nest(-idProperty, .key = dataLeaseDetails)
          return(df)
        }
        
        if (column == "features") {
          list_cols <- names(df)
          
          df <- 
            list_cols %>% 
            map_dfr(function(list_column){
              list_column %>% message()
              value <- 
                df[[list_column]] %>% 
                discard(function(x){x %>% is_null()}) %>% 
                flatten_chr()
              tibble(item = list_column, value)
            })
          
          df <- df %>% 
            mutate(idProperty) %>% 
            nest(-idProperty, .key = dataFeatures)
          
          return(df)
        }
        
        if (column == "floorplans") {
          df <- 
            df %>% 
            purrr::set_names(df %>% .realtor_names()) %>% 
            remove_na() %>% 
            dplyr::select(-dplyr::matches("remove"))

          if (df %>% tibble::has_name("dateAvailable")){
            df <- 
              df %>% 
              rename(available = dateAvailable)
          }
          df <- 
            df %>%
            .munge_realtor()
          
          if (df %>% tibble::has_name("idProperty")) {
            df <- df %>% rename(idPropertyListing = idProperty)
          }
          
          df <- 
            df %>% 
            mutate(idProperty)
          
          if (df %>% tibble::has_name("dateAvailable")){
            df <- 
              df %>% 
              rename(dateAvailable = available) %>% 
              mutate(dateAvailable = lubridate::ymd(dateAvailable))
          }
          
          df <- 
            df %>% 
            nest(-idProperty, .key = dataFloorPlans)
          return(df)
        }
        
        if (column == "tax_history") {
          actual_names <- 
            c("yearTaxes",
          "amountTax",
          "amountLandAssessment",
          "amountBuildingAssessment",
          "amountTotalAssessment",
          "remove_amountyear_display",
          "remove_amounttax_display",
          "remove_amountland_assessment_display",
          "remove_amountbuilding_assessment_display",
          "remove_amounttotal_assessment_display")[1:length(names(df))]
          
          df <-
            df %>% 
            purrr::set_names(actual_names) %>% 
            dplyr::select(-dplyr::matches("remove")) %>% 
            mutate(idProperty) %>%
            .munge_realtor() %>% 
            nest(-idProperty, .key = dataTaxHistory)
          return(df)
        }
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
        
        if (column == "style") {
          df <- 
            df %>% 
            rename(typeHomeStyle = name,
                   slugURLStyle = url)
          df <-
            df %>%
            .munge_realtor() %>%
            mutate(idProperty) %>%
            nest(-idProperty, .key = dataListingHistory)
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
            dplyr::select(-dplyr::matches("remove"))
          
          df <-
            df %>%
            .munge_realtor() %>%
            mutate(idProperty) %>%
            remove_na() %>% 
            nest(-idProperty, .key = dataPriceHistory)
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
            seq_along(feature_names) %>%
            future_map_dfr(function(x) {
              feature_name <-
                feature_names[[x]]
              flattened_feature <-
                df[x][[feature_name]]
              if (flattened_feature %>% purrr::is_null()) {
                return(tibble())
              }
              
              if (class(flattened_feature) == "logical") {
                return(tibble())
              }
              
              items <-
                flattened_feature  %>% discard(purrr::is_null) %>% flatten_chr()
              if (items %>% length() == 0) {
                return(tibble())
              }
              tibble(typeFeature = feature_name, nameFeature = items)
            })
          
          if (nrow(df) == 0) {
            return(tibble())
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
      left_join(data_cols) %>%
      suppressMessages()
    
    df_list_cols <-
      df_prop_cols %>%
      filter(value %in% c("list")) %>%
      filter(!item %in% c("saved_resource_note", "saved_listings","lead_attributes")) %>%
      pull(item)
    
    
    data_list_cols <-
      df_list_cols %>% 
      map(function(column) {
        column %>% cat(fill = T)
        if (json_property[[column]] %>% length() == 0) {
          return(invisible())
        }
        
        if (column == "linking_modules") {
          df <- json_property[[column]]
          df_cities <- 
            df$linking_cities %>% 
            gather(nameCity, slugRealtor, na.rm = T) %>% 
            as_tibble()
          df_hoods <-
            df$linking_neighborhoods %>% 
            gather(nameNeighborHood, slugRealtor, na.rm = T) %>% 
            as_tibble()
          
          df <- 
            tibble(idProperty, dataCitiesLinked = list(df_cities), dataNeighborhoodsLinked = list(df_hoods))
          
          return(df)
          
        }
        
        if (column == "around_neighborhood") {
          df <-
            json_property[[column]]
          
          df_hoods <- df$nearby_neighborhoods
          
          df_areas <- df$other_areas
          df_areas <- 
            df_areas %>% 
            purrr::set_names(df_areas %>% .resolve_names()) %>% 
            .munge_realtor()
          
          if (length(df_hoods) > 0) {
            df_hoods <- 
              df_hoods %>% flatten_df()
            
            df_hoods <- 
              df_hoods %>% 
              purrr::set_names(df_hoods %>% .resolve_names()) %>% 
              .munge_realtor()
            
            df_areas <- 
              df_areas %>% 
              mutate(dataNeighborhoods = list(df_hoods))
          }
          
          df_areas <- 
            df_areas %>% 
            mutate(idProperty) %>% 
            nest(-idProperty, .key = dataAreas)
          return(df_areas)
        }
        
        if (column == "rentals_with_gym") {
          df <- json_property[[column]]
          df <- 
            df$properties
          
          if (df %>% tibble::has_name("search_flags")) {
            df$search_flags <- NULL
          }
          
          df <- 
            df %>%
            mutate_if(is.character,
                      funs(ifelse(. %in% c("", " "), NA, .))) %>% 
            remove_na() %>% 
            as_tibble()
          
          df <- df %>% 
            purrr::set_names(df %>% .resolve_names())
          
          df <- 
            df %>% 
            .munge_realtor()
          
          if (df %>% tibble::has_name("idProperty")) {
            df <- df %>% 
              rename(idPropertyListing = idProperty)
          }
          
          df <- 
            df %>% 
            mutate(idProperty) %>% 
            nest(-idProperty, .key = dataBuildingsGyms)
          
          return(df)
        }
        
        if (column == "pet_friendly_rentals") {
          df <- json_property[[column]]
          df <- 
            df$properties
          
          if (df %>% tibble::has_name("search_flags")) {
            df$search_flags <- NULL
          }
          
          df <- 
            df %>%
            mutate_if(is.character,
                      funs(ifelse(. %in% c("", " "), NA, .))) %>% 
            remove_na() %>% 
            as_tibble()
          
          df <- df %>% 
            purrr::set_names(df %>% .resolve_names())
          
          df <- 
            df %>% 
            .munge_realtor()
          
          if (df %>% tibble::has_name("idProperty")) {
            df <- df %>% 
              rename(idPropertyListing = idProperty)
          }
          
          df <- 
            df %>% 
            mutate(idProperty) %>% 
            nest(-idProperty, .key = dataBuildingsPetFriendly)
          
          return(df)
        }
        
        if (column == "rentals_with_in_laundry_unit") {
          df <- json_property[[column]]
          df <- 
            df$properties
          
          if (df %>% tibble::has_name("search_flags")) {
            df$search_flags <- NULL
          }
          
          df <- 
            df %>%
            mutate_if(is.character,
                      funs(ifelse(. %in% c("", " "), NA, .))) %>% 
            remove_na() %>% 
            as_tibble()
          
          df <- df %>% 
            purrr::set_names(df %>% .resolve_names())
          
          df <- 
            df %>% 
            .munge_realtor()
          
          if (df %>% tibble::has_name("idProperty")) {
            df <- df %>% 
              rename(idPropertyListing = idProperty)
          }
          
          df <- 
            df %>% 
            mutate(idProperty) %>% 
            nest(-idProperty, .key = dataBuildingsLaundry)
          
          return(df)
        }
        
        if (column == "listing_provider") {
          df <- 
            json_property[[column]]
          
          df_class <- df %>% map_df(class) %>% gather(column, class)
          
          good_cols <- 
            df_class %>% filter(!class %in% c("list", "data.frame", "NULL")) %>% pull(column)
          
          df_listing <- df[names(df) %in% good_cols] %>% flatten_df()
          
          actual_names <-
            .realtor_names(data = df_listing)
          
          
          
          df_listing <-
            df_listing %>% purrr::set_names(actual_names) %>%
            mutate(idProperty) %>%
            remove_na() %>% 
            nest(-idProperty, .key = dataBrokerListing)
          return(df_listing)
        }
        
        if (column == "schools_info") {
          df <-
            json_property[[column]]
          df_private <- df$nearby_schools$private_school
          df_assigned <- df$assigned_schools
          df_assigned <- df_assigned %>% select(-education_level)
          df_elementry <- df$nearby_schools$public_elementary_school
          return(invisible())
        }
        
        if (!column %in% c("branding", "ldp_urls")) {
          df <-
            json_property[[column]] %>% flatten_df()
          
          if (nrow(df) == 0) {
            return(invisible())
          }
        }
        
        
        if (column == "pets_display") {
          pet_status <- df %>% pull(value) %>% str_detect("OK")
          type <- df %>% pull(label)
          df <- 
            tibble(typePet = type, isPetAllowed = pet_status) %>% 
            mutate(idProperty) %>% 
            nest(-idProperty, .key = dataPets)
          return(df)
        }
        
        if (column == "timeline_details") {
          actual_names <-
            .realtor_names(data = df)
          
          df <-
            df %>%
            purrr::set_names(actual_names) %>%
            mutate(idProperty) %>%
            .munge_realtor() %>% 
            remove_na() %>% 
            nest(-idProperty, .key = "dataTimeLine")
          return(df)
        }
        if (column == "avm_trend") {
          actual_names <-
            .realtor_names(data = df)
          
          df <-
            df %>%
            purrr::set_names(actual_names) %>%
            .munge_realtor() %>% 
            remove_na() %>% 
            mutate(idProperty) %>%
            nest(-idProperty, .key = "dataAVM")
          return(df)
        }
        
        if (column == "property_insights") {
          actual_names <-
            .realtor_names(data = df)
          
          df <-
            df %>%
            purrr::set_names(actual_names) %>%
            mutate(idProperty) %>%
            .munge_realtor() %>% 
            remove_na() %>% 
            nest(-idProperty, .key = "dataPropertyInsights")
          return(df)
        }
        
        
        if (column == "recently_sold_around") {
          actual_names <-
            .realtor_names(data = df)
          
          df <-
            df %>%
            purrr::set_names(actual_names) %>%
            mutate(idProperty) %>%
            .munge_realtor() %>% 
            remove_na() %>% 
            nest(-idProperty, .key = "dataRecentHomeSales")
          return(df)
        }
        
        if (column == "raw_photo") {
          actual_names <-
            .realtor_names(data = df)
          
          df <-
            df %>%
            purrr::set_names(actual_names) %>%
            mutate(idProperty) %>%
            nest(-idProperty, .key = "dataPhotoPrimary")
          return(df)
        }
        
        if (column == "building") {
          actual_names <-
            .realtor_names(data = df)
          
          df <-
            df %>%
            purrr::set_names(actual_names)
          
          if (df %>% tibble::has_name("idProperty")) {
            df <- df %>% rename(idBuilding = idProperty)
          }
          df <-
            df %>%
            mutate(idProperty) %>%
            nest(-idProperty, .key = "dataBuilding")
          return(df)
        }
        
        if (column == "tax_history") {
          actual_names <-
            .realtor_names(data = df)
          
          df <-
            df %>%
            purrr::set_names(actual_names) %>%
            .munge_realtor() %>% 
            remove_na() %>% 
            mutate(idProperty) %>%
            nest(-idProperty, .key = "dataTaxHistory")
          return(df)
        }
        
        if (column == "home_values_around") {
          actual_names <-
            .realtor_names(data = df)
          
          df <-
            df %>%
            purrr::set_names(actual_names) %>%
            .munge_realtor() %>% 
            remove_na() %>% 
            mutate(idProperty) %>%
            nest(-idProperty, .key = "dataHomeValuesAround")
          return(df)
        }
        
        if (column == "similar_homes") {
          actual_names <-
            .realtor_names(data = df)
          
          df <-
            df %>%
            purrr::set_names(actual_names) %>%
            .munge_realtor() %>% 
            remove_na() %>% 
            mutate(idProperty) %>%
            nest(-idProperty, .key = "dataSimilarHomes")
          return(df)
        }
        
        if (column == "similar_homes_nearby") {
          actual_names <-
            .realtor_names(data = df)
          
          df <-
            df %>%
            purrr::set_names(actual_names) %>%
            .munge_realtor() %>% 
            remove_na() %>% 
            mutate(idProperty) %>%
            nest(-idProperty, .key = "dataSimilarHomesNearBy")
          return(df)
        }
        
        if (column == "popular_homes") {
          actual_names <-
            .realtor_names(data = df)
          
          df <-
            df %>%
            purrr::set_names(actual_names) %>%
            .munge_realtor() %>% 
            remove_na() %>% 
            mutate(idProperty) %>%
            nest(-idProperty, .key = "dataPopularHomes")
          return(df)
        }
        
        if (column == "rentals_around") {
          actual_names <-
            .realtor_names(data = df)
          
          df <-
            df %>%
            purrr::set_names(actual_names) %>%
            .munge_realtor() %>% 
            remove_na() %>% 
            mutate(idProperty) %>%
            nest(-idProperty, .key = "dataRentalsAround")
          return(df)
        }
        
        if (column == "market_value_summary") {
          actual_names <-
            .realtor_names(data = df)
          
          df <-
            df %>%
            purrr::set_names(actual_names) %>%
            .munge_realtor() %>% 
            remove_na() %>% 
            mutate(idProperty) %>%
            nest(-idProperty, .key = "dataMarketValueSummary")
          return(df)
        }
        
        if (column == "public_record") {
          df <- json_property[[column]] %>% flatten_df()
          actual_names <-
            .realtor_names(data = df)
          
          df <- df %>%
            purrr::set_names(actual_names) %>%
            .munge_realtor() %>% 
            mutate(idProperty) %>%
            nest(-idProperty, .key = "dataPublicRecord")
          return(df)
        }
        
        if (column == "flags") {
          df <- json_property[[column]] %>% flatten_df()
          actual_names <-
            .realtor_names(data = df)
          
          df <- df %>%
            purrr::set_names(actual_names) %>%
            mutate(idProperty) %>%
            .munge_realtor() %>% 
            remove_na() %>% 
            nest(-idProperty, .key = "dataPropertyFlags")
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
          json_property[[column]] %>% as_tibble()
        
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
          feature_names <-
            names(df)
          
          df <-
            seq_along(feature_names) %>%
            future_map(function(x) {
              feature_name <-
                feature_names[[x]]
              feature_name %>% cat(fill = T)
              df_feature <-
                df[x][[feature_name]] %>% flatten() %>% discard(purrr::is_null) %>% flatten_df() %>% select(-one_of("show_realtor_logo"))
              
              if (ncol(df_feature) == 0) {
                return(invisible())
              }
              
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
            discard(purrr::is_null) %>%
            purrr::reduce(left_join) %>%
            suppressMessages()
          
          names(df) <- names(df) %>% str_replace_all("link", "url")
          df <- df %>%
            mutate(idProperty) %>%
            nest(-idProperty, .key = "dataBrokerBranding")
          return(df)
        }
        
      }) %>%
      discard(purrr::is_null)
    
    data_list_cols <- 
      data_list_cols %>% 
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
    property_id <-
      url %>% str_remove_all("https://www.realtor.com/property-overview/M") %>% as.numeric()
    
    json_data <-
      .curl_json(url = url) %>%
      jsonlite::fromJSON(simplifyVector = T, simplifyDataFrame = T) 
    
    data <- 
      .munge_listing_api(json_data = json_data, property_id = property_id) %>% suppressWarnings() %>%
      mutate(urlPropertyAPI = url)
    
    if (!sleep_time %>% purrr::is_null()) {
      Sys.sleep(time = sleep_time)
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
#' @return a \code{tibble}
#' @export
#'
#' @examples
parse_listing_urls <-
  function(urls = NULL,
           sleep_time = 1,
           assign_to_environment = F,
           return_message = TRUE) {
    
    .parse_listing_api_url_safe <-
      purrr::possibly(.parse_listing_api_url, tibble())
    
    all_data <-
      urls %>%
      future_map_dfr(function(url) {
        if (return_message) {
          glue::glue(
            "Parsing {url %>% str_replace_all('https://www.realtor.com/property-overview/', '')}"
          ) %>%
            cat(fill = T)
        }
        .parse_listing_api_url_safe(url = url,
                                    sleep_time = sleep_time) %>%
          suppressWarnings()
      }) %>% 
      suppressWarnings()
    
    
    if (assign_to_environment) {
      df_base <- all_data %>% select(-matches("data"))
      
      assign(x = "df_base_listings", df_base, envir = .GlobalEnv)
      
      data_columns <- 
        all_data %>% select(matches("data")) %>% names()
      
      data_columns %>% 
        walk(function(data_column){
          slug <- data_column %>% str_remove_all("data") %>% str_to_lower()
          glue::glue("Assigning listing {slug}") %>% message()
          table_name <- 
            glue::glue("df_{slug}_listings") %>% 
            as.character()
          
          df <- 
            all_data %>% 
            dplyr::select(one_of("idProperty", "statusProperty", "typeProperty", data_column)) %>% 
            rename(data := UQ(data_column)) %>% 
            mutate(hasData = data %>% map_dbl(length) > 0) %>% 
            filter(hasData) %>% 
            select(-hasData) %>% 
            unnest()
          
          assign(x = table_name, df, envir = .GlobalEnv)
          
        })
      
    }
    
    all_data
  }