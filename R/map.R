

.generate_headers <-
  function() {
    df_headers <-
      .headers_base()
    df_call <- generate_url_reference()
    df_headers <-
      df_headers %>%
      mutate(`user-agent` = df_call$userAgent)
    df_headers
  }


.parse_data_properties <-
  function(data_properties) {
    all_results <-
      seq_along(data_properties) %>%
      map_df(function(x) {
        glue::glue("Parsing {x}") %>% message()
        data_row <- data_properties[[x]]
        df_col_types <-
          data_row %>% map(class) %>% as_data_frame() %>%
          gather(column, type)
        
        remove <-
          df_col_types %>% filter(type == "NULL") %>% pull(column)
        
        data_row <- data_row[!data_row %>% names() %in% remove]
        
        df_base_cols <-
          df_col_types %>%
          filter(!type %in% c("NULL", "list"))
        
        df_list_cols <-
          df_col_types %>%
          filter(type %in% 'list')
        
        df_base <-
          data_row[names(data_row) %in% df_base_cols$column] %>%
          flatten_df()
        
        df_list <-
          data_row[names(data_row) %in% df_list_cols$column] %>%
          purrr::flatten()
        
        df_list_class <-
          df_list %>%
          map(class) %>%
          as_data_frame() %>%
          gather(column, class)
        
        if (df_list_class %>% filter(class == "NULL") %>% nrow() > 0) {
          null_cols <-
            df_list_class %>% filter(class == "NULL") %>% pull(column)
          df_list <-
            df_list[!df_list %>% names() %in% null_cols]
        }
        
        if (df_list_class %>% filter(column == 'coordinates') %>% nrow() > 0) {
          data_list <-
            df_list %>%
            flatten_df()
          names(data_list)[1:2] <-
            c('lon', 'lat')
        } else {
          data_list <-
            df_list %>%
            flatten_df()
        }
        
        df <-
          df_base %>%
          bind_cols(data_list) %>%
          mutate(numberProperty = x) %>%
          select(numberProperty, everything())
        
        df
        
        
      })
    
    all_results <-
      all_results %>%
      dplyr::select(-matches("display")) %>%
      dplyr::select(-one_of(c("id", "type", "plot")))
    
    df_names <- dictionary_realtor_names()
    
    actual_names <-
      names(all_results) %>%
      map_chr(function(name) {
        df_row <- df_names %>% filter(nameRealtor == name)
        if (df_row %>% nrow() == 0) {
          glue::glue("Missing {name}") %>%
            message()
          return(name)
        }
        df_row %>%  pull(nameActual)
      })
    
    all_results <-
      all_results %>%
      set_names(actual_names) %>%
      dplyr::select(-matches("remove"))
    
    all_results <-
      all_results %>%
      mutate(urlListing = 'https://www.realtor.com' %>% str_c(slugLDP)) %>%
      select(-slugLDP) %>%
      .munge_realtor()
    
    if (all_results %>% tibble::has_name('areaPropertySF')) {
      all_results <-
        all_results %>%
        mutate(priceListingPerSF = priceListing / areaPropertySF)
    }
    
    
    all_results
  }

.parse_data_parameters <-
  function(data_param) {
    df_class <-
      data_param %>% map(class) %>% flatten_df() %>%
      gather(column, class)
    
    df_base_names <-
      df_class %>%
      filter(!class %in% c("NULL", "list")) %>%
      pull(column)
    
    df_params <-
      data_param[names(data_param) %in% df_base_names] %>% flatten_df() %>%
      gather(item, value) %>%
      mutate_all(funs(ifelse(. == '', NA_character_, .))) %>%
      filter(!is.na(value)) %>%
      left_join(dictionary_search() %>% rename(item = column)) %>%
      select(nameActual, value) %>%
      spread(nameActual, value) %>%
      suppressMessages()
    
    list_cols <- df_base_names <-
      df_class %>%
      filter(class %in% c("list")) %>%
      pull(column)
    
    df_list <- data_param[names(data_param) %in% list_cols]
    df_facets <- df_list$facets
    df_facets[!names(df_facets) %in% c("features_hash")]
    df_list <-
      df_facets[!names(df_facets) %in% c("features_hash")] %>%
      flatten_df() %>%
      gather(item, value) %>%
      mutate_all(funs(ifelse(. == '', NA_character_, .))) %>%
      filter(!is.na(value)) %>%
      left_join(dictionary_search() %>% rename(item = column)) %>%
      select(nameActual, value) %>%
      spread(nameActual, value) %>%
      suppressWarnings() %>%
      suppressMessages()
    
    if (df_facets$features_hash %>% length() > 0) {
      df_list <-
        df_list %>%
        mutate(listFeatures = df_facets$features_hash %>% str_c(collapse = ", "))
    }
    
    if (df_list %>% ncol() > 0) {
      df_params <-
        df_params %>%
        bind_cols(df_list)
    }
    
    df_params %>%
      select(-matches("remove_")) %>%
      .munge_realtor()
  }

dictionary_search <-
  function() {
    data_frame(
      column = c(
        "search_criteria",
        "city",
        "county",
        "discovery_mode",
        "state",
        "postal",
        "sort",
        "position",
        "facets",
        "search_controller",
        "neighborhood",
        "street",
        "searchType",
        "school",
        "types",
        "searchFacetsToDTM",
        "searchFeaturesToDTM",
        "pos",
        "page_size",
        "viewport_height",
        "pin_height",
        "page",
        "beds_min",
        "beds_max",
        "baths_min",
        "baths_max",
        "price_min",
        "price_max",
        "prop_type",
        "sqft_min",
        "sqft_max",
        "acre_min",
        "acre_max",
        "lot_unit",
        "age_max",
        "age_min",
        "radius",
        "pets",
        "days_on_market",
        "open_house",
        "show_listings",
        "pending",
        "foreclosure",
        "new_construction",
        "multi_search",
        "include_pending_contingency",
        "features_hash"
      ),
      nameActual = c(
        "locationSearch",
        "citySearch",
        "countySearch",
        "isDiscoveryMode",
        "stateSearch",
        "zipcodeSearch",
        "remove_sort",
        "remove_position",
        "listFacets",
        "remove_search_controller",
        "neighborhoodSearch",
        "streetSearch",
        "typeSearch",
        "schoolSearch",
        "typeProperty",
        "remove_searchFacetsToDTM",
        "remove_searchFeaturesToDTM",
        "remove_pos",
        "remove_zie_page",
        "remove_viewport_height",
        "remove_pin_height",
        "numberPage",
        "countBedsMin",
        "countBedsMax",
        "countBathsMin",
        "countBathsMax",
        "priceMin",
        "priceMax",
        "typeProperty",
        "areaSFMin",
        "areaSFMax",
        "areaLandAcreMin",
        "areaLandAcreMax",
        "lotUnit",
        "ageMax",
        "ageMin",
        "radiusMiles",
        "hasPets",
        "countDaysOnMarket",
        "hasOpenHouse",
        "isShowListing",
        "isPending",
        "isForeclosure",
        "isNewConstruction",
        "isMultiSearch",
        "hasPendings",
        "listFeatures"
      )
      
    )
  }

dictionary_features <-
  function() {
    data_frame(
      nameFeature = c(
        "Pool",
        "Spa",
        "Waterfront",
        "Basement",
        "2 Car Garage",
        "Single Story",
        "Multifamily"
      ),
      slugFeature = c(
        "swimming_pool" ,
        "spa_or_hot_tub",
        "waterfront",
        "basement",
        "garage_2_or_more",
        "single_story",
        "two_or_more_stories"
      )
    )
  }

.headers_base <-
  function() {
    structure(
      list(
        cookie = "threshold_value=56; automation=false; clstr=v; clstr_tcv=14; __vst=06687a89-26aa-424d-8d01-4b45dba07097; __ssn=358102aa-3f72-4ee6-855c-b5fcc80b4a25; __ssnstarttime=1520516724; basecamp=false; ajs_user_id=null; ajs_group_id=null; ajs_anonymous_id=%22f6e9aa90-48d4-4348-8ab9-c0bfa18c7593%22; AMCVS_8853394255142B6A0A4C98A4%40AdobeOrg=1; far_geo=%7B%22CityState%22%3A%7B%22City%22%3A%22Marietta%22%2C%22SearchAreaID%22%3A114%2C%22StateID%22%3A%22GA%22%7D%2C%22CityStateSearch%22%3Afalse%2C%22ConfidenceLevel%22%3A0%2C%22Country%22%3A%22USA%22%2C%22Intersection%22%3A%22%22%2C%22Latitude%22%3A33.927089%2C%22Longitude%22%3A-84.541084%2C%22MatchedMethod%22%3A3%2C%22PostalCode%22%3A%2230060%22%2C%22PostalCodeSearch%22%3Atrue%2C%22Street%22%3A%22%22%7D; _agent-profile_session=SGRBeHNiQ0gvWGx6b0F6VVdCSlF3aVFac0M4WkZiTlBHT3ZwQ0ptUXN4YktiS1BsL2NPckt6MVVZWU93YXBzZ1NHSGxyZFVqek5PU3JKbG1aeS9Vd2IrVlg0NEFVeGpUVTJPK0xPN1htWWk0Ylo2b2p0OTM2TURaci9pcko2dDM2MnBESVpOZjF2VHN1OGg3aU9ST2d3PT0tLWFMaFlGdndMR25JVWt4allBQjF5b3c9PQ%3D%3D--f9e08c793eeb963927612fce658aff9db51418fe; search_params=%7B%22geo_slug%22%3A%2230060%22%2C%22_pjax%22%3A%22%23pjax-container%22%2C%22agent_rating_min%22%3A%225%22%2C%22has_photo%22%3A%221%22%2C%22price_range%22%3A%22500000_7000000%22%7D; srp.viewType=map; criteria=loc%3DMarietta%2C+GA%26locSlug%3DMarietta_GA%26lat%3D33.967466%26long%3D-84.521937%26status%3D1%26pos%3D33.632572%2C-85.211782%2C34.246659%2C-84.071951%2C10%26pg%3D1%26pgsz%3D15%26sprefix%3D%2Frealestateandhomes-search%26city%3DMarietta%26state_id%3DGA%26county_fips%3D13067%26county_fips_multi%3D13067-13057-13121; srchID=b2e1aea3f17045a8afa891a6476b9d24; AMCV_8853394255142B6A0A4C98A4%40AdobeOrg=-179204249%7CMCIDTS%7C17599%7CMCMID%7C44784613493650775057191498471160544248%7CMCAID%7CNONE%7CMCOPTOUT-1520546418s%7CNONE; header_slugs=gs%3DMarietta_GA%26lo%3DMarietta%26st%3Dcity%2Cgs%3DCobb-County_GA%26lo%3DCobb%26st%3Dcity; _rdc-next_session=SlJZdWxsdHhJdkY0WmVMVTF4SjBHVklJLzdQNTJyNjJ0dlNWK1J6TGo2ck1GVDNnNGRWbitrelZuQ010MDFXUk43WjkzTFhuMEVENkRvYWhwYW9zTnp2ZTF4a0pLbDdlTTFBSWpPOXJDRmh3ZmVGM0lVWFRDRmQ3K0VzdUFiV2hpMm81Vlo1TUhsUy9LdFJHd3RpUXZnPT0tLU9ZbW9xLy9LalB5UU5La0VpZDZFdGc9PQ%3D%3D--5e31ae7054e456e659789555eebafab0925c5602",
        origin = "https://www.realtor.com",
        `accept-encoding` = "gzip, deflate, br",
        `x-csrf-token` = "PDN4JVcUZeeXyI2vgIJfUx0mpgj65VfxL0fgtdF1neE/VgMi8U1V7cT6bfdkdgWAP77baGC49jAtiECyjuq2xw==",
        `accept-language` = "en-US,en;q=0.9",
        `x-requested-with` = "XMLHttpRequest",
        `x-newrelic-id` = "VwEPVF5XGwYEV1JaDwAD",
        `user-agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/65.0.3325.124 Safari/537.36",
        `content-type` = "application/json",
        accept = "application/json, text/javascript, */*; q=0.01",
        referer = "https://www.compass.com/",
        authority = "www.realtor.com",
        dnt = "0"
      ),
      .Names = c(
        "cookie",
        "origin",
        "accept-encoding",
        "x-csrf-token",
        "accept-language",
        "x-requested-with",
        "x-newrelic-id",
        "user-agent",
        "content-type",
        "accept",
        "referer",
        "authority",
        "dnt"
      ),
      row.names = c(NA, -1L),
      class = c("tbl_df", "tbl", "data.frame")
    )
    
  }

.data_base <-
  function() {
    structure(
      list(
        search_criteria = "location_slug",
        city = "",
        county = "",
        discovery_mode = TRUE,
        state = "",
        postal = NULL,
        sort = NULL,
        position = NULL,
        facets = structure(
          list(
            beds_min = NULL,
            beds_max = NULL,
            baths_min = NULL,
            baths_max = NULL,
            price_min = NULL,
            price_max = NULL,
            prop_type = "",
            sqft_min = NULL,
            sqft_max = NULL,
            acre_min = NULL,
            acre_max = NULL,
            lot_unit = NULL,
            age_max = NULL,
            age_min = NULL,
            radius = NULL,
            pets = NULL,
            days_on_market = NULL,
            open_house = NULL,
            show_listings = NULL,
            pending = NULL,
            foreclosure = NULL,
            new_construction = NULL,
            multi_search = structure(list(), .Names = character(0)),
            include_pending_contingency = TRUE,
            features_hash = list()
          ),
          .Names = c(
            "beds_min",
            "beds_max",
            "baths_min",
            "baths_max",
            "price_min",
            "price_max",
            "prop_type",
            "sqft_min",
            "sqft_max",
            "acre_min",
            "acre_max",
            "lot_unit",
            "age_max",
            "age_min",
            "radius",
            "pets",
            "days_on_market",
            "open_house",
            "show_listings",
            "pending",
            "foreclosure",
            "new_construction",
            "multi_search",
            "include_pending_contingency",
            "features_hash"
          )
        ),
        search_controller = "Search::PropertiesController",
        neighborhood = NULL,
        street = NULL,
        searchType = "city",
        school = NULL,
        types = "property",
        searchFacetsToDTM = "pf_not_visible",
        searchFeaturesToDTM = list(),
        pos = "",
        page_size = 2500L,
        viewport_height = 1000L,
        pin_height = 240L,
        page = 1L
      ),
      .Names = c(
        "search_criteria",
        "city",
        "county",
        "discovery_mode",
        "state",
        "postal",
        "sort",
        "position",
        "facets",
        "search_controller",
        "neighborhood",
        "street",
        "searchType",
        "school",
        "types",
        "searchFacetsToDTM",
        "searchFeaturesToDTM",
        "pos",
        "page_size",
        "viewport_height",
        "pin_height",
        'page'
      )
    )
    
  }



.generate_data <-
  function(location_name,
           search_type = "city",
           page = 1,
           city_isolated = NULL,
           county_isolated = NULL,
           zipcode_isolated = NULL,
           state_isolated = NULL,
           street_isolated = NULL,
           neighborhood_isolated = NULL,
           beds_min = NULL,
           beds_max = NULL,
           baths_min = NULL,
           baths_max = NULL,
           features = NULL,
           price_min = NULL,
           price_max = NULL,
           property_type = NULL,
           sqft_min = NULL,
           sqft_max = NULL,
           acre_min = NULL,
           acre_max = NULL,
           age_min = NULL,
           age_max = NULL,
           days_on_market = NULL,
           pending = NULL,
           only_open_houses = NULL,
           is_new_construction =  NULL,
           is_foreclosure = NULL,
           include_pending_contingency = TRUE) {
    options(scipen = 99999)
    location_name <- as.character(location_name)
    
    data <-
      .data_base()
    
    df_loc_val <-
      validate_locations(locations = location_name, return_message = F)
    
    data$search_criteria <-
      df_loc_val$slugLocation
    
    if (!city_isolated %>% purrr::is_null()) {
      data$city  <- city_isolated
    }
    
    if (!county_isolated %>% purrr::is_null()) {
      data$county <- county_isolated
    }
    
    if (!zipcode_isolated %>% purrr::is_null()) {
      data$postal <- zipcode_isolated
    }
    
    if (!state_isolated %>% purrr::is_null()) {
      data$state <- state_isolated
    }
    
    if (!neighborhood_isolated %>% purrr::is_null()) {
      data$neighborhood <- neighborhood_isolated
    }
    if (!street_isolated %>% purrr::is_null()) {
      data$street <- street_isolated
    }
    
    if (!only_open_houses %>% purrr::is_null()) {
      data$show_listings <-
        'oh'
    }
    
    
    if (!beds_min %>% purrr::is_null()) {
      data$facets$beds_min <-
        as.character(beds_min)
    }
    
    if (!beds_max %>% purrr::is_null()) {
      data$facets$beds_max <-
        as.character(beds_max)
    }
    if (!baths_min %>% purrr::is_null()) {
      data$facets$baths_min <-
        as.character(baths_min)
    }
    
    if (!baths_max %>% purrr::is_null()) {
      data$facets$baths_max <-
        as.character(baths_max)
    }
    
    if (!features %>% purrr::is_null()) {
      data$facets$features_hash <-
        c(features)
    }
    
    if (!price_min %>% purrr::is_null()) {
      data$facets$price_min <-
        as.character(price_min)
    }
    
    if (!price_max %>% purrr::is_null()) {
      data$facets$price_max <-
        as.character(price_max)
    }
    
    
    if (!property_type %>% purrr::is_null()) {
      data$facets$prop_type <- property_type
    }
    
    if (!sqft_min %>% purrr::is_null()) {
      data$facets$sqft_min <-
        sqft_min
    }
    
    if (!sqft_max %>% purrr::is_null()) {
      data$facets$sqft_max <- sqft_max
    }
    
    
    if (!acre_min %>% purrr::is_null()) {
      data$facets$acre_min <- acre_min
    }
    
    if (!acre_max %>% purrr::is_null()) {
      data$facets$acre_max <- acre_max
    }
    
    if (!days_on_market %>% purrr::is_null()) {
      data$facets$days_on_market <- days_on_market
    }
    
    if (!pending %>% purrr::is_null()) {
      data$facets$pending <- pending
    }
    
    if (!is_new_construction %>% purrr::is_null()) {
      data$facets$new_construction <- is_new_construction
    }
    
    
    if (!age_max %>% purrr::is_null()) {
      data$facets$age_max <- age_max
    }
    
    if (!age_min %>% purrr::is_null()) {
      data$facets$age_min <- age_min
    }
    
    if (!is_foreclosure %>% purrr::is_null()) {
      data$facets$foreclosure <- is_foreclosure
    }
    
    if (!include_pending_contingency %>% purrr::is_null()) {
      data$facets$include_pending_contingency <-
        include_pending_contingency
    }
    data$page <- page
    
    data
  }

.get_location_counts <-
  function(location_name = 10016,
           search_type = "city",
           city_isolated = NULL,
           county_isolated = NULL,
           zipcode_isolated = NULL,
           state_isolated = NULL,
           street_isolated = NULL,
           neighborhood_isolated = NULL,
           features = NULL,
           beds_min = NULL,
           only_open_houses = F,
           beds_max = NULL,
           baths_min = NULL,
           baths_max = NULL,
           price_min = NULL,
           price_max = NULL,
           property_type = NULL,
           sqft_min = NULL,
           sqft_max = NULL,
           acre_min = NULL,
           acre_max = NULL,
           age_min = NULL,
           age_max = NULL,
           days_on_market = NULL,
           pending = NULL,
           is_new_construction =  NULL,
           is_foreclosure = NULL,
           include_pending_contingency = TRUE) {
    url <- "https://www.realtor.com/search_result_count"
    headers =  .generate_headers()
    
    data <-
      .generate_data(
        location_name = location_name,
        search_type = search_type,
        page = 1,
        city_isolated = city_isolated,
        county_isolated = county_isolated,
        zipcode_isolated = zipcode_isolated,
        state_isolated = state_isolated,
        only_open_houses = only_open_houses,
        street_isolated = street_isolated,
        neighborhood_isolated = neighborhood_isolated,
        beds_min = beds_min,
        beds_max = beds_max,
        baths_min = baths_min ,
        baths_max = baths_max,
        price_min = price_min,
        price_max = price_max,
        features = features,
        property_type = property_type,
        sqft_min = sqft_min,
        sqft_max = sqft_max,
        acre_min = acre_min,
        acre_max = acre_max,
        age_min = age_min,
        age_max = age_max,
        days_on_market = days_on_market,
        pending = pending,
        is_new_construction = is_new_construction,
        is_foreclosure = is_foreclosure,
        include_pending_contingency = include_pending_contingency
      )
    
    df_params <-
      .parse_data_parameters(data_param = data)
    
    response  <-
      Post(
        url = url,
        headers = headers,
        data = data %>% toJSON(auto_unbox = T)
      )
    
    json_data <-
      response$content %>%
      fromJSON(flatten = T, simplifyVector = T)
    
    count <-
      json_data$properties_count
    
    df_loc_val <-
      validate_locations(locations = location_name)
    
    df_loc_val <-
      df_loc_val %>%
      mutate(countListings = count)
    
    df_loc_val <-
      df_loc_val %>%
      mutate(id = 1) %>%
      left_join(df_params %>% mutate(id = 1) %>% select(-locationSearch)) %>%
      select(locationSearch, one_of(names(df_params)), everything()) %>%
      select(-id) %>%
      suppressMessages()
    
    df_loc_val
  }

#' Title
#'
#' @param locations
#' @param search_type
#' @param city_isolated
#' @param county_isolated
#' @param zipcode_isolated
#' @param state_isolated
#' @param street_isolated
#' @param neighborhood_isolated
#' @param beds_min
#' @param beds_max
#' @param baths_min
#' @param baths_max
#' @param price_min
#' @param price_max
#' @param property_type
#' @param sqft_min
#' @param sqft_max
#' @param acre_min
#' @param acre_max
#' @param age_min
#' @param age_max
#' @param days_on_market
#' @param pending
#' @param is_new_construction
#' @param is_foreclosure
#' @param include_pending_contingency
#'
#' @return
#' @export
#'
#' @examples
listing_counts <-
  function(locations ,
           search_type = "city",
           city_isolated = NULL,
           county_isolated = NULL,
           zipcode_isolated = NULL,
           state_isolated = NULL,
           street_isolated = NULL,
           neighborhood_isolated = NULL,
           beds_min = NULL,
           beds_max = NULL,
           baths_min = NULL,
           baths_max = NULL,
           price_min = NULL,
           price_max = NULL,
           only_open_houses = NULL,
           property_type = NULL,
           sqft_min = NULL,
           sqft_max = NULL,
           acre_min = NULL,
           acre_max = NULL,
           age_min = NULL,
           age_max = NULL,
           days_on_market = NULL,
           pending = NULL,
           is_new_construction =  NULL,
           is_foreclosure = NULL,
           include_pending_contingency = TRUE) {
    .get_location_counts_safe <-
      purrr::possibly(.get_location_counts, data_frame())
    locations %>%
      map_df(function(location) {
        .get_location_counts_safe(
          location_name = location,
          search_type = search_type,
          city_isolated = city_isolated,
          county_isolated = county_isolated,
          zipcode_isolated = zipcode_isolated,
          state_isolated = state_isolated,
          street_isolated = street_isolated,
          neighborhood_isolated = neighborhood_isolated,
          beds_min = beds_min,
          beds_max = beds_max,
          baths_min = baths_min ,
          baths_max = baths_max,
          price_min = price_min,
          price_max = price_max,
          property_type = property_type,
          sqft_min = sqft_min,
          sqft_max = sqft_max,
          acre_min = acre_min,
          acre_max = acre_max,
          age_min = age_min,
          age_max = age_max,
          days_on_market = days_on_market,
          pending = pending,
          is_new_construction = is_new_construction,
          is_foreclosure = is_foreclosure,
          include_pending_contingency = include_pending_contingency
        )
      })
  }

.get_location_listings_json <-
  function(location_name = 10016,
           search_type = "city",
           city_isolated = NULL,
           county_isolated = NULL,
           zipcode_isolated = NULL,
           state_isolated = NULL,
           street_isolated = NULL,
           features = NULL,
           only_open_houses = NULL,
           neighborhood_isolated = NULL,
           beds_min = NULL,
           beds_max = NULL,
           baths_min = NULL,
           baths_max = NULL,
           price_min = NULL,
           price_max = NULL,
           property_type = NULL,
           sqft_min = NULL,
           sqft_max = NULL,
           acre_min = NULL,
           acre_max = NULL,
           age_min = NULL,
           age_max = NULL,
           days_on_market = NULL,
           pending = NULL,
           is_new_construction =  NULL,
           is_foreclosure = NULL,
           include_pending_contingency = TRUE) {
    df_count <-
      .get_location_counts(
        location_name = location_name,
        search_type = search_type,
        city_isolated = city_isolated,
        county_isolated = county_isolated,
        zipcode_isolated = zipcode_isolated,
        state_isolated = state_isolated,
        street_isolated = street_isolated,
        neighborhood_isolated = neighborhood_isolated,
        beds_min = beds_min,
        beds_max = beds_max,
        baths_min = baths_min ,
        baths_max = baths_max,
        price_min = price_min,
        price_max = price_max,
        property_type = property_type,
        sqft_min = sqft_min,
        sqft_max = sqft_max,
        acre_min = acre_min,
        acre_max = acre_max,
        age_min = age_min,
        age_max = age_max,
        days_on_market = days_on_market,
        pending = pending,
        is_new_construction = is_new_construction,
        is_foreclosure = is_foreclosure,
        include_pending_contingency = include_pending_contingency,
        features = features,
        only_open_houses = only_open_houses
      )
    
    pages <-
      df_count$countListings %/% 50
    
    pages <- max(1, pages)
    
    all_properties <-
      1:pages %>%
      map_df(function(page) {
        glue::glue("Parsing page {page} of {pages} for location {location_name}") %>% message()
        
        data <-
          .generate_data(
            location_name = location_name,
            search_type = search_type,
            page = page,
            city_isolated = city_isolated,
            county_isolated = county_isolated,
            zipcode_isolated = zipcode_isolated,
            state_isolated = state_isolated,
            street_isolated = street_isolated,
            neighborhood_isolated = neighborhood_isolated,
            beds_min = beds_min,
            beds_max = beds_max,
            baths_min = baths_min ,
            baths_max = baths_max,
            price_min = price_min,
            price_max = price_max,
            property_type = property_type,
            sqft_min = sqft_min,
            sqft_max = sqft_max,
            acre_min = acre_min,
            acre_max = acre_max,
            age_min = age_min,
            age_max = age_max,
            days_on_market = days_on_market,
            pending = pending,
            is_new_construction = is_new_construction,
            is_foreclosure = is_foreclosure,
            include_pending_contingency = include_pending_contingency,
            features = features,
            only_open_houses = only_open_houses
          )
        
        df_params <-
          .parse_data_parameters(data_param = data)
        
        headers <-
          .generate_headers()
        
        response  <-
          Post(
            url = "https://www.realtor.com/search_result.json",
            headers = headers,
            data = data %>% toJSON(auto_unbox = T)
          )
        
        json_data <-
          response$content %>%
          fromJSON(flatten = T, simplifyVector = T)
        
        
        data_properties <- json_data$results$property$items
        
        all_data <-
          data_properties %>%
          .parse_data_properties() %>%
          mutate(numberPage = page)
        
        all_data <-
          all_data %>%
          left_join(df_params %>% mutate(numberPage = as.numeric(numberPage))) %>%
          select(names(df_params), everything()) %>%
          suppressMessages()
        
        all_data
      })
    
    all_properties %>%
      select(-numberPage) %>%
      distinct()
  }

#' Map listing data
#'
#' @param locations
#' @param search_type
#' @param city_isolated
#' @param county_isolated
#' @param zipcode_isolated
#' @param state_isolated
#' @param street_isolated
#' @param features
#' @param only_open_houses
#' @param neighborhood_isolated
#' @param beds_min
#' @param beds_max
#' @param baths_min
#' @param baths_max
#' @param price_min
#' @param price_max
#' @param property_type
#' @param sqft_min
#' @param sqft_max
#' @param acre_min
#' @param acre_max
#' @param age_min
#' @param age_max
#' @param days_on_market
#' @param pending
#' @param is_new_construction
#' @param is_foreclosure
#' @param include_pending_contingency
#'
#' @return
#' @export
#'
#' @examples
map_listings <-
  function(locations = NULL,
           search_type = "city",
           city_isolated = NULL,
           county_isolated = NULL,
           zipcode_isolated = NULL,
           state_isolated = NULL,
           street_isolated = NULL,
           features = NULL,
           only_open_houses = NULL,
           neighborhood_isolated = NULL,
           beds_min = NULL,
           beds_max = NULL,
           baths_min = NULL,
           baths_max = NULL,
           price_min = NULL,
           price_max = NULL,
           property_type = NULL,
           sqft_min = NULL,
           sqft_max = NULL,
           acre_min = NULL,
           acre_max = NULL,
           age_min = NULL,
           age_max = NULL,
           days_on_market = NULL,
           pending = NULL,
           is_new_construction =  NULL,
           is_foreclosure = NULL,
           include_pending_contingency = TRUE) {
    .get_location_listings_json_safe <-
      purrr::possibly(.get_location_listings_json, data_frame())
    
    all_data <- 
      locations %>%
      map_df(function(location) {
        .get_location_listings_json(
          location_name = location,
          search_type = search_type,
          city_isolated = city_isolated,
          county_isolated = county_isolated,
          zipcode_isolated = zipcode_isolated,
          state_isolated = state_isolated,
          street_isolated = street_isolated,
          neighborhood_isolated = neighborhood_isolated,
          beds_min = beds_min,
          beds_max = beds_max,
          baths_min = baths_min ,
          baths_max = baths_max,
          price_min = price_min,
          price_max = price_max,
          property_type = property_type,
          sqft_min = sqft_min,
          sqft_max = sqft_max,
          acre_min = acre_min,
          acre_max = acre_max,
          age_min = age_min,
          age_max = age_max,
          days_on_market = days_on_market,
          pending = pending,
          is_new_construction = is_new_construction,
          is_foreclosure = is_foreclosure,
          include_pending_contingency = include_pending_contingency,
          features = features,
          only_open_houses = only_open_houses
        )
      }) %>% 
      remove_na()
    
    all_data
  }



.headers_search_json_base <-
  function() {
    structure(
      list(
        cookie = "threshold_value=19; automation=false; clstr=n1; clstr_tcv=7; __vst=657f20bc-079c-4563-8471-adcfaa0e6610; __ssn=d53e9ebc-204b-4b57-a4bb-75a6a6b02731; __ssnstarttime=1520691388; basecamp=false; bcc=false; ajs_user_id=null; ajs_group_id=null; ajs_anonymous_id=%227aae3639-1cd2-4639-81ad-2ef60b8f42ff%22; gpl=v1; seen_ny_prop=true; buyer=false; AMCVS_8853394255142B6A0A4C98A4%40AdobeOrg=1; AMCV_8853394255142B6A0A4C98A4%40AdobeOrg=-179204249%7CMCIDTS%7C17601%7CMCMID%7C37907106676684272897760290470733580491%7CMCAID%7CNONE%7CMCOPTOUT-1520702593s%7CNONE; header_slugs=gs%3DQueens-County_NY%26lo%3DQueens%26st%3Dcounty; criteria=loc%3DQueens+County%2C+NY%26locSlug%3DQueens-County_NY%26lat%3D40.657513%26long%3D-73.838803%26status%3D1%26sl%3Dnc%26pg%3D1%26pgsz%3D15%26features%3Ds2%26sprefix%3D%2Frealestateandhomes-search%26city%3DQueens+County%26state_id%3DNY; srchID=c72a73c777bb485a8fee8f6a1ff4239b; _rdc-next_session=M3JvOFdNVnhuSE1iVmJkcXM5akZCZGk1QzRPUzdwdGhRSmc5ZUVpUitmYU5BUWFVckZFWWZ5NC9tWjVuNEdvaUxBN0dRRVM4SGI3dDJLL283Kzl4OGQ0OGF1RGZqZXlpTmF5ejROY3ZPL1lvQjQrOHE2eU42c3VVbWlhb3V3ZnRzWFM2SkhVUGRlU3pLK1FIUlFSOVZ3PT0tLU5QdlZVTmlzdkIvL3h2Rkw3Q0FMY3c9PQ%3D%3D--cfcc77729e49e8c6623ff7828c1ddf89b1edeae5",
        origin = "https://www.realtor.com",
        `accept-encoding` = "gzip, deflate, br",
        `x-csrf-token` = "Ni86Jtz9U1QtskRgX5dsT2TJuSi885v9nPQMpImiZrWrNN8lFa30jdz/FhqQL8znbXG+lNUbaScjD5zcM/P+zw==",
        `accept-language` = "en-US,en;q=0.9",
        `x-requested-with` = "XMLHttpRequest",
        `x-newrelic-id` = "VwEPVF5XGwYEV1JaDwAD",
        `user-agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/65.0.3325.124 Safari/537.36",
        `content-type` = "application/json",
        accept = "text/html, */*; q=0.01",
        referer = "https://nra.com/",
        authority = "www.realtor.com",
        dnt = "0"
      ),
      .Names = c(
        "cookie",
        "origin",
        "accept-encoding",
        "x-csrf-token",
        "accept-language",
        "x-requested-with",
        "x-newrelic-id",
        "user-agent",
        "content-type",
        "accept",
        "referer",
        "authority",
        "dnt"
      ),
      row.names = c(NA, -1L),
      class = c("tbl_df", "tbl", "data.frame")
    )
    
  }

.headers_search_json <-
  function() {
    df_call <- generate_url_reference()
    
    .headers_search_json_base() %>%
      mutate(`user-agent` = df_call$userAgent)
  }



.get_location_listings <-
  function(location_name = 10016,
           search_type = "city",
           city_isolated = NULL,
           county_isolated = NULL,
           zipcode_isolated = NULL,
           state_isolated = NULL,
           street_isolated = NULL,
           features = NULL,
           only_open_houses = NULL,
           neighborhood_isolated = NULL,
           beds_min = NULL,
           beds_max = NULL,
           baths_min = NULL,
           baths_max = NULL,
           price_min = NULL,
           price_max = NULL,
           property_type = NULL,
           sqft_min = NULL,
           sqft_max = NULL,
           acre_min = NULL,
           acre_max = NULL,
           age_min = NULL,
           age_max = NULL,
           days_on_market = NULL,
           pending = NULL,
           is_new_construction =  NULL,
           is_foreclosure = NULL,
           include_pending_contingency = TRUE) {
    df_count <-
      .get_location_counts(
        location_name = location_name,
        search_type = search_type,
        city_isolated = city_isolated,
        county_isolated = county_isolated,
        zipcode_isolated = zipcode_isolated,
        state_isolated = state_isolated,
        street_isolated = street_isolated,
        neighborhood_isolated = neighborhood_isolated,
        beds_min = beds_min,
        beds_max = beds_max,
        baths_min = baths_min ,
        baths_max = baths_max,
        price_min = price_min,
        price_max = price_max,
        property_type = property_type,
        sqft_min = sqft_min,
        sqft_max = sqft_max,
        acre_min = acre_min,
        acre_max = acre_max,
        age_min = age_min,
        age_max = age_max,
        days_on_market = days_on_market,
        pending = pending,
        is_new_construction = is_new_construction,
        is_foreclosure = is_foreclosure,
        include_pending_contingency = include_pending_contingency,
        features = features,
        only_open_houses = only_open_houses
      )
    
    pages <-
      df_count$countListings %/% 50
    
    pages <- max(1, pages)
    
    headers <-
      .headers_search_json()
    
    all_properties <-
      1:pages %>%
      map_df(function(page_no) {
        glue::glue("Parsing page {page_no} of {pages} for location {location_name}") %>% message()
        
        if (page_no == 1) {
          url <-  "https://www.realtor.com/search_result"
        } else {
          url <- 'https://www.realtor.com/pagination_result'
        }
        
        data <-
          .generate_data(
            location_name = location_name,
            search_type = search_type,
            page = page_no,
            city_isolated = city_isolated,
            county_isolated = county_isolated,
            zipcode_isolated = zipcode_isolated,
            state_isolated = state_isolated,
            street_isolated = street_isolated,
            neighborhood_isolated = neighborhood_isolated,
            beds_min = beds_min,
            beds_max = beds_max,
            baths_min = baths_min ,
            baths_max = baths_max,
            price_min = price_min,
            price_max = price_max,
            property_type = property_type,
            sqft_min = sqft_min,
            sqft_max = sqft_max,
            acre_min = acre_min,
            acre_max = acre_max,
            age_min = age_min,
            age_max = age_max,
            days_on_market = days_on_market,
            pending = pending,
            is_new_construction = is_new_construction,
            is_foreclosure = is_foreclosure,
            include_pending_contingency = include_pending_contingency,
            features = features,
            only_open_houses = only_open_houses
          )
        
        df_params <-
          .parse_data_parameters(data_param = data)
        
        page  <-
          Post(
            url = url,
            headers = headers,
            data = data %>% toJSON(auto_unbox = T),
            parse_html = T
          )
        
        fact_nodes <-
          page %>%
          html_nodes('li')
        
        
        data_nodes <-
          fact_nodes %>%
          html_nodes(xpath = "//li[contains(concat(' ', @class, ' '), ' js-record-user-activity')]")
        
        lat_lon_nodes <-
          data_nodes %>%
          html_nodes(xpath = "//div[contains(concat(' ', @class, ' '), 'listing-geo sr-only')]")
        
        hood_nodes <-
          data_nodes %>%
          html_nodes(xpath = "//div[contains(concat(' ', @class, ' '), 'srp-item-neighborhood ellipsis link-tertiary hidden-xxs hidden-xs-small')]")
        
        data_prop <-
          seq_along(data_nodes) %>%
          map_df(function(x) {
            fact_node <-
              data_nodes[[x]]
            
            lat_lon_node <-
              lat_lon_nodes[[x]]
            
            #hood_node <-
            # hood_nodes[[x]]
            
            # neighborhoods <-
            #  hood_node %>%
            # html_nodes("a") %>%
            #html_text() %>%
            #   str_c(collapse = ", ")
            
            lat_lon <-
              lat_lon_node %>%
              html_nodes('meta') %>%
              html_attrs() %>%
              flatten()
            
            df_lat_lon <-
              data_frame(
                name =  c('latitude', 'longitude'),
                value = c(lat_lon[[1]], lat_lon[[3]])
              )
            
            fact_children <-
              fact_node %>%
              html_children()
            
            data_atrs <- fact_node %>% html_attrs()
            
            df_attrs <-
              data_frame(name = names(data_atrs),
                         value = data_atrs %>% as.character())
            
            df_json_rows <-
              df_attrs %>%
              filter(name %in% c("data-lead_attributes", "data-search_flags"))
            
            
            
            df_base <-
              df_attrs %>%
              filter(!name %in%  c("class", "data-lead_attributes", "data-search_flags")) %>%
              bind_rows(df_lat_lon)
            
            property_features <-
              fact_node %>%
              html_nodes('.srp-item-property-meta') %>%
              html_nodes('li')
            
            type_listing <-
              fact_node %>%
              html_nodes('.srp-property-type') %>%
              html_text()
            
            status_listing <-
              fact_node %>%
              html_nodes('.label-wrapper span') %>%
              html_text() %>%
              str_trim()
            
            
            # if (neighborhoods %>% length() > 0 ) {
            ##   df_base %>%
            #  bind_rows(
            #   data_frame(name = "nameNeighborhoods", value = neighborhoods)
            #)
            #           }
            if (type_listing %>% length() > 0) {
              df_base <-
                df_base %>%
                bind_rows(data_frame(name = "typeListing", value = type_listing))
            }
            
            if (type_listing %>% length() > 0) {
              df_base <-
                df_base %>%
                bind_rows(data_frame(name = "statusListing", value = status_listing))
            }
            
            if (property_features %>% length() > 0) {
              name <-
                property_features %>% html_attrs() %>% flatten_chr()
              value <-
                property_features %>% html_text()
              df_base <-
                df_base %>%
                bind_rows(data_frame(name, value))
            }
            
            if (df_json_rows %>% nrow() > 0) {
              df_json_data <-
                1:nrow(df_json_rows) %>%
                map_df(function(x) {
                  df_json_rows %>% dplyr::slice(x) %>% pull(value) %>% jsonlite::fromJSON() %>%
                    flatten_df() %>%
                    mutate_all(as.character) %>%
                    gather(name, value)
                })
              
              df_base <-
                df_base %>%
                bind_rows(df_json_data)
              
            }
            
            
            address_nodes <-
              fact_node %>%
              html_nodes(xpath = 'span') %>%
              html_children()
            
            df_address <-
              seq_along(address_nodes) %>%
              map_df(function(x) {
                name <-
                  address_nodes[x] %>% html_attrs() %>%
                  .[[1]] %>% as.character()
                value <-
                  address_nodes[x] %>% html_text()
                data_frame(name, value)
              })
            
            if (fact_node %>% html_nodes('.srp-item-broker-text-only span') %>% html_text() %>% length() > 0) {
              df_base <-
                df_base %>%
                bind_rows(data_frame(
                  name = c("broker"),
                  value = fact_node %>% html_nodes('.srp-item-broker-text-only span') %>% html_text()
                ))
            }
            
            has_image <-
              fact_node %>%
              html_nodes('img') %>% length() > 0
            
            if (has_image) {
              image_node <-
                fact_node %>%
                html_nodes('img')
              
              image_url <-
                image_node %>%
                html_attr('src') %>%
                .[[1]]
              
              address <-
                image_node %>%
                html_attr('title') %>%
                .[[1]]
              
              df_base <- df_base %>%
                bind_rows(data_frame(
                  name = c('addressPropertyFull', 'urlImage'),
                  value = c(address, image_url)
                ))
            }
            
            df_base <-
              df_base %>%
              bind_rows(df_address) %>%
              mutate(numberListing = x) %>%
              select(numberListing, everything())
            
            df_base
            
          })
        
        data_prop <-
          data_prop %>%
          left_join(dictionary_css_page() %>% rename(name = id)) %>%
          select(numberListing, nameActual, value) %>%
          mutate_all(funs(ifelse(. == "", NA_character_, .))) %>%
          filter(!is.na(value)) %>%
          filter(!nameActual %>% str_detect("remove_")) %>%
          distinct() %>%
          group_by(numberListing, nameActual) %>%
          mutate(id = 1:n()) %>%
          ungroup() %>%
          filter(id == min(id)) %>%
          select(-id) %>%
          spread(nameActual, value) %>%
          .munge_realtor() %>%
          suppressMessages() %>%
          mutate(numberPage = page_no) %>%
          select(numberPage, everything())
        data_prop
      })
    
    df_count_merge <-
      df_count %>% select(-one_of(
        c(
          "cityProperty",
          "urlListing",
          "countListings",
          "numberPage",
          "typeProperty"
        )
      ))
    
    all_data <-
      all_properties %>%
      mutate(id = 1) %>%
      left_join(df_count_merge %>% mutate(id = 1)) %>%
      select(-id) %>%
      select(one_of(names(df_count_merge)), everything()) %>%
      select(-numberPage) %>%
      distinct() %>%
      suppressMessages()
    
    all_data <-
      all_data %>%
      mutate(urlListing = urlListing %>%  gsub("https://www.realtor.com//", "https://www.realtor.com/", .))
    
    all_data
  }


#' MLS listing data
#'
#' Returns MLS data for
#' specified locations and parameters
#'
#' @param locations
#' @param search_type
#' @param city_isolated
#' @param county_isolated
#' @param zipcode_isolated
#' @param state_isolated
#' @param street_isolated
#' @param features
#' @param only_open_houses
#' @param neighborhood_isolated
#' @param beds_min
#' @param beds_max
#' @param baths_min
#' @param baths_max
#' @param price_min
#' @param price_max
#' @param property_type
#' @param sqft_min
#' @param sqft_max
#' @param acre_min
#' @param acre_max
#' @param age_min
#' @param age_max
#' @param days_on_market
#' @param pending
#' @param is_new_construction
#' @param is_foreclosure
#' @param include_pending_contingency
#'
#' @return
#' @export
#' @examples
listings <-
  function(locations = NULL,
           search_type = "city",
           city_isolated = NULL,
           county_isolated = NULL,
           zipcode_isolated = NULL,
           state_isolated = NULL,
           street_isolated = NULL,
           features = NULL,
           only_open_houses = NULL,
           neighborhood_isolated = NULL,
           beds_min = NULL,
           beds_max = NULL,
           baths_min = NULL,
           baths_max = NULL,
           price_min = NULL,
           price_max = NULL,
           property_type = NULL,
           sqft_min = NULL,
           sqft_max = NULL,
           acre_min = NULL,
           acre_max = NULL,
           age_min = NULL,
           age_max = NULL,
           days_on_market = NULL,
           pending = NULL,
           is_new_construction =  NULL,
           is_foreclosure = NULL,
           include_pending_contingency = TRUE) {
    if (locations %>% purrr::is_null()) {
      stop("Enter locations")
    }
    .get_location_listings_safe <-
      purrr::possibly(.get_location_listings,
                      data_frame())
    
    all_data <-
      locations %>%
      map_df(function(location) {
        .get_location_listings_safe(
          location_name = location,
          search_type = search_type,
          city_isolated = city_isolated,
          county_isolated = county_isolated,
          zipcode_isolated = zipcode_isolated,
          state_isolated = state_isolated,
          street_isolated = street_isolated,
          neighborhood_isolated = neighborhood_isolated,
          beds_min = beds_min,
          beds_max = beds_max,
          baths_min = baths_min ,
          baths_max = baths_max,
          price_min = price_min,
          price_max = price_max,
          property_type = property_type,
          sqft_min = sqft_min,
          sqft_max = sqft_max,
          acre_min = acre_min,
          acre_max = acre_max,
          age_min = age_min,
          age_max = age_max,
          days_on_market = days_on_market,
          pending = pending,
          is_new_construction = is_new_construction,
          is_foreclosure = is_foreclosure,
          include_pending_contingency = include_pending_contingency,
          features = features,
          only_open_houses = only_open_houses
        )
      })
    
    all_data %>%
      remove_columns()
  }