# gdeltr2::load_needed_packages(c("jsonlite", "purrr", "tidyr", "glue", "stringr", "curl", "dplyr", "rvest", 'lubridate', "requestsR"))

.curl_page <-
  function(url) {
    df_call <- generate_url_reference()
    h <-
      
      curl::new_handle(
        accept_encoding = NULL,
        verbose = F,
        useragent =  df_call$urlReferer
      )
    
    page <-
      curl::curl(url, handle = h) %>%
      readr::read_lines() %>%
      str_c(collapse = "") %>%
      xml2::read_html()
    
    page
  }

.curl_json <-
  function(url) {
    df_call <- generate_url_reference()
    h <-
      curl::new_handle(
        accept_encoding = NULL,
        verbose = F,
        useragent =  df_call$urlReferer
      )
    
    json_data <-
      curl::curl(url, handle = h) %>%
      readr::read_lines()
        json_data
    
  }

# munge -------------------------------------------------------------------

.munge_realtor <-
  function(data) {
    num_names <-
      data %>% dplyr::select(
        matches(
          "^area|^count[A-Z]|^price|^latitude|^longitude|^year|^index|^id[A-Z]|^number[A-Z]|^size"
        )
      ) %>%
      dplyr::select(-matches("countyProperty")) %>%
      names()
    
    
    if (num_names %>% length() > 0) {
      data <-
        data %>%
        mutate_at(num_names,
                  funs(. %>% readr::parse_number()))
      
    }
    
    log_names <-
      data %>% dplyr::select(matches("^is|^has")) %>% names()
    
    data <-
      data %>%
      mutate_at(log_names,
                funs(as.logical))
    
    if (data %>% tibble::has_name("slugLDP")) {
      data <-
        data %>%
        mutate(urlListing = glue::glue("https://www.realtor.com/{slugLDP}") %>% as.character()) %>%
        dplyr::select(-slugLDP)
      
    }
    
    if (data %>% tibble::has_name("areaPropertySF") &&
        data %>%  tibble::has_name("priceDisplay")) {
      data <-
        data %>%
        mutate(pricePerSFListing = (priceDisplay / areaPropertySF) %>% round(digits = 2))
    }
    
    if (data %>% tibble::has_name("areaPropertySF") &&
        data %>%  tibble::has_name("priceListing")) {
      data <-
        data %>%
        mutate(pricePerSFListing = (priceListing / areaPropertySF) %>% round(digits = 2))
    }
    
    if (data %>% tibble::has_name("nameBrokerage")) {
      data <-
        data %>%
        mutate(nameBrokerage = nameBrokerage %>% str_to_upper())
      
      data <-
        data %>%
        separate(
          col = nameBrokerage,
          into = c("nameBrokerage", "locationOffice"),
          sep = "\\-"
        ) %>%
        suppressWarnings() %>%
        mutate_if(is.character,
                  str_trim)
      
    }
    
    data <-
      data %>%
      mutate_if(is.character,
                funs(ifelse(. == '', NA_character_, .))) %>%
      remove_na()
    
    
    data
  }

# dict --------------------------------------------------------------------
dictionary_css_page <-
  function() {
    data_frame(
      id = c(
        "ra ra-status-sale",
        "ra ra-price-per-sq-ft",
        "ra ra-days-on-realtor",
        "ra ra-property-type",
        "ra ra-year-built",
        "ra ra-home-style",
        "data-advertiserid-agent",
        "data-advertiserid-office",
        "data-advertiserid-broker",
        "data-listingid",
        "data-omtag",
        "data-page",
        "data-producttype",
        "data-proptype",
        "data-propertytype",
        "data-propertyid",
        "data-status",
        "data-url",
        "data-hot-buy",
        "id",
        "itemscope",
        "data-promotion_status",
        "itemtype",
        "data-price",
        "data-product_name",
        "data-baths_full",
        "data-baths_half",
        "data-baths",
        "data-lot_size",
        "data-leadfm_ab_test",
        "add-lead-form-modal",
        "is_cobroke",
        "is_showcase",
        "is_suppress_map_pin",
        "is_address_suppressed",
        "is_foreclosure",
        "is_advantage_pro",
        "show_contact_a_lender",
        "lead_type",
        "form_type",
        "broker",
        "streetAddress",
        "addressLocality",
        "addressRegion",
        "postalCode",
        "latitude",
        "longitude",
        "addressProperty",
        "urlImage",
        "property-meta-beds",
        "property-meta-baths",
        "property-meta-sqft",
        "property-meta-lotsize",
        "property-meta-garage",
        "typeListing",
        "addressPropertyFull",
        "data-agentid",
        "property-meta-build",
        "statusListing",
        "statusListingDetail",
        "branding-agent-name",
        "branding-agent-phone",
        "branding-office-name",
        "branding-office-phone",
        "property-meta-bath",
        "property-baths-count"
        
        
      ),
      nameActual = c(
        'statusListing',
        'pricePerSF',
        'countDaysOnRealtor',
        'typePropertyDetail',
        'yearBuilt',
        'styleHome',
        "idAgent",
        "idOffice",
        "idBroker",
        "idListing",
        "remove_omtag",
        "remove_page",
        "remove_producttype",
        "remove_proptype",
        "typeProperty",
        "idProperty",
        "statusProperty",
        "slugLDP",
        "slugHotBuy",
        "remove_id",
        "typePropertySchema",
        "statusPromotion",
        "remove_schema",
        "priceListing",
        "slugProduct",
        "countBathsFull",
        "countBathsHalf",
        "remove_baths",
        "areaLotSize",
        "remove_ab_tst",
        "remove_form_modal",
        "isCoBroke",
        "isShowcased",
        "isSuppressdPin",
        "isAddressSuppress",
        "isForeclosure",
        "isAdvantagPro",
        "remove_show_contact_a_lender",
        "slugLead",
        "slugForm",
        "nameBrokerage",
        "addressProperty",
        "cityProperty",
        "stateProperty",
        "zipcodeProperty",
        "latitudeProperty",
        "longitudeProperty",
        "addressProperty",
        "urlImage",
        "countBeds",
        "countBaths",
        "areaPropertySF",
        "sizeLotAcres",
        "typeGarage" ,
        "typeListing",
        "addressPropertyFull",
        "idAgent",
        "statusPropertyBuild",
        "statusListing",
        "statusListingDetail",
        "nameAgent",
        "phoneAgent",
        "nameBrokerage",
        "phoneBrokerage",
        "countBaths",
        "countBaths"
      )
    )
  }

dictionary_realtor_names <-
  function() {
    data_frame(
      nameRealtor = c(
        "price_per_sqft",
        "median_listing_price",
        "median_rent_price",
        "for_sale_count",
        "new_listing_count",
        "open_house_count",
        "recently_sold_count",
        "foreclosure_count",
        "price_reduced_count",
        "ldp_url",
        "photo_url",
        "address_display",
        "beds",
        "baths",
        "sqft_display",
        "price_display",
        "listing_id",
        "property_id",
        "status",
        "saved_resource_id",
        'full_address_display',
        'address',
        'city',
        'state_code',
        'postal_code',
        'lat',
        'lon',
        'agent_id',
        'property_type',
        'days_on_realtor',
        'baths_half',
        'baths_full',
        'neighborhood_highlight',
        'price_for_highlight',
        'age_on_market_or_realtor',
        'search_flags.is_price_reduced',
        'search_flags.is_new_listing',
        'search_flags.is_pending',
        'search_flags.is_contingent',
        'url',
        'location',
        'slug',
        'validatedLocation',
        'state_id',
        "zip",
        "year",
        "month",
        "active_listing_count",
        "hotmarket_rank",
        "hotmarket_index",
        "hotmarket_temperature",
        "asking_to_sold_price_percent_change",
        "sales_to_inventory_count_percent",
        "sales_to_inventory_count_percent_market_type",
        "median.age_days",
        "median.listing_price",
        "median.rental_listing_price",
        "median.listing_price_sqft",
        "median.closing_price",
        "listing_count.for_sale",
        "listing_count.rental",
        "listing_count.sold",
        'numberProperty',
        'ldpUrl',
        'source_id',
        'isSaved',
        'mprId',
        'officeName',
        'product_type',
        'sort',
        'isCoBroke',
        'isEnhanced',
        'isForeclosure',
        'isPriceReduced',
        'isAddressSuppressed',
        'bed',
        'bath',
        'lotSize',
        'photo',
        'photoCount',
        'price',
        'propertyType',
        'sqft',
        'county',
        'state',
        'is_price_reduced',
        'is_pending',
        'is_contingent',
        'is_new_listing',
        'is_showcase',
        'has_tour',
        'has_video',
        'has_realtor_logo',
        'is_cobroke',
        'is_coshow_product',
        'is_foreclosure',
        'is_new_plan',
        'price_excludes_land',
        'has_deals',
        'is_new',
        'is_costar',
        'is_aptlist',
        'is_address_suppressed',
        'is_suppress_map_pin',
        'is_suppress_map',
        'is_advantage',
        'is_advantage_brand',
        'is_advantage_pro',
        'is_essentials',
        'is_basic',
        'is_basic_opt_in',
        'is_basic_opt_out',
        'has_matterport',
        'agentId',
        'is_short_sale'
      ),
      nameActual = c(
        "pricePerSF",
        "priceListingMedian",
        "priceRentMedian",
        "countForSale",
        "countNewListings",
        "countOpenHouse",
        "countSoldRecently",
        "countForeclosures",
        "countPriceReduced",
        "slugLDP",
        "urlPhoto",
        "addressDisplay",
        "countBeds",
        "countBaths",
        "areaPropertySF",
        "priceListing",
        "idListing",
        "idProperty",
        "statusListing",
        "idResouceSaved",
        'addressDisplayFull',
        'addressProperty',
        'cityProperty',
        'stateProperty',
        'zipcodeProperty',
        'latitudeProperty',
        'longitudeProperty',
        'idAgent',
        'typeProperty',
        'countDaysOnRealtor',
        'countBathsHalf',
        'countBathsFull',
        'slugNeighborhoodHighlight',
        'priceHighlight',
        'remove',
        'isPriceReduced',
        'isListingNew',
        'isPendingSale',
        'isContingent',
        'slugLDP',
        'nameLocation',
        'slugLocation',
        'nameLocationValidated',
        'slugState',
        "zipcodeProperty",
        "yearData",
        "monthData",
        "countActiveListings",
        "rankMarketHot",
        "indexMarketTemperature",
        "temperatureMarket",
        "pctSoldPriceToAsk",
        "pctSalesToInventory",
        "typeSalesToInventoryFavor",
        "countDaysMarketMedian",
        "priceListingMedian",
        "priceRentMedian",
        "pricePerSFMedian",
        "priceClosingMedian",
        "countForSale",
        "countRental",
        "countSold",
        
        'numberProperty',
        'slugLDP',
        'slugSource',
        'isSaved',
        'idMPR',
        'nameBrokerage',
        'typeListing',
        'numberSort',
        'isCoBroke',
        'isEnhanced',
        'remove_foreclosure',
        'isPriceReduced',
        'remove_address_suppressed',
        'countBeds',
        'countBaths',
        'areaLotSF',
        'urlPhoto',
        'countPhotos',
        'priceListing',
        'typeProperty',
        'areaPropertySF',
        'countyProperty',
        'stateProperty',
        'remove_reduced',
        'isPending',
        'isContingent',
        'isListingNew',
        'isShowcased',
        'hasTour',
        'hasVideo',
        'hasRealtorLogo',
        'remove_cobroke',
        'isCoShow',
        'isForeclosure',
        'isPlanNew',
        'isPriceExLand',
        'hasDeals',
        'isNew',
        'isCostar',
        'isAptList',
        'isAddressSuppressed',
        'isSuppressMapPin',
        'isSuppressMap',
        'isAdvantage',
        'isAdvantageBrand',
        'isAdvantagePro',
        'isEssentials',
        'isBasic',
        'isBasicOptIn',
        'isBasicOptOut',
        'hasMatterPort',
        'idAgent',
        'isShortSale'
      )
    )
  }


# rates -------------------------------------------------------------------

# https://www.realtor.com/mrtg_handler/get_trends_data

#' Motgage Rates
#'
#' Returns a variety of
#' interest rates for various
#' mortgage types.
#'
#' @param return_wide if \code{TRUE} widens data and removes duration and benchmark variables
#'
#' @return a \code{data_frame}
#' @export
#' @family interest rates
#' @examples
#' mortage_rates(return_wide = F)
mortage_rates <-
  function(return_wide = F) {
    data <-
      "https://www.realtor.com/mrtg_handler/get_trends_data" %>%
      jsonlite::fromJSON(flatten = T, simplifyDataFrame = T) %>%
      .$rate_trends %>%
      dplyr::as_data_frame()
    
    df_types <-
      data_frame(
        typeRate = c(
          'pct30YearFixed',
          'pct15YearFixed',
          'pct5OneARM',
          'pct20YearFixed',
          'pct30YearFixedFHA',
          'pct30YearFixedVA',
          'pct10YearFixed',
          'pct7OneARM'
        ),
        durationLoanMonths = c(360,
                               15 * 12,
                               5 * 12,
                               20 * 12,
                               360,
                               360,
                               120,
                               84),
        typeBenchmark = c(
          'fixed',
          'fixed',
          'floating',
          'fixed',
          'fixed',
          'fixed',
          'fixed',
          'floating'
        )
      )
    
    data <-
      data %>%
      purrr::set_names(
        c(
          "year",
          "month",
          "date",
          'pct30YearFixed',
          'pct15YearFixed',
          'pct5OneARM',
          'pct20YearFixed',
          'pct30YeaFixedFHA',
          'pct30YearFixedFHA',
          'pct10YearFixed',
          'pct7OneARM'
        )
      ) %>%
      tidyr::unite(dateData, year, month, date, sep = "-") %>%
      mutate(dateData = dateData %>% lubridate::ymd()) %>%
      gather(typeRate, value, -dateData) %>%
      mutate(value = value / 100) %>%
      arrange(dateData)
    
    data <-
      data %>%
      left_join(df_types) %>%
      select(dateData, typeRate, durationLoanMonths, typeBenchmark, value) %>%
      suppressMessages()
    
    if (return_wide) {
      data <-
        data %>%
        select(-one_of(c(
          "durationLoanMonths", "typeBenchmark"
        ))) %>%
        spread(typeRate, value)
    }
    data
  }



# market data -------------------------------------------------------------

# https://www.realtor.com/median_prices?city=Bethesda&state_code=MD

parse_location <-
  function(location_name = c("Bethesda, MD")) {
    if (location_name %>% str_detect("\\,")) {
      is_city_state <-
        location_name %>% str_count("\\,") == 1
      if (is_city_state) {
        location_slugs <-
          location_name %>% str_split("\\,") %>% flatten_chr() %>% str_trim()
        
        data <-
          data_frame(
            locationSearch = location_name,
            citySearch = location_slugs[[1]],
            stateSearch = location_slugs[[2]]
          )
        return(data)
        
      } else {
        location_slugs <-
          location_name %>% str_split("\\,") %>% flatten_chr() %>% str_trim()
        
        data <-
          data_frame(
            locationSearch = location_name,
            streetOrNeighborhoodSearch = location_slugs[[1]],
            citySearch = location_slugs[[2]],
            stateSearch = location_slugs[[3]]
          )
        
        return(data)
      }
    }
    
    data_frame(locationSearch = location_name, zipcodeSearch = location_name)
    
    
  }

.generate_market_url <-
  function(location_name = c("Bethesda, MD")) {
    df_loc_slug <-
      location_name %>%
      as.character() %>%
      str_trim() %>%
      parse_location()
    
    city <-
      df_loc_slug$citySearch
    
    city_slug <-
      city %>% URLencode()
    state <- df_loc_slug$stateSearch
    
    if (state %>% nchar() > 4) {
      stop("Has to be the 2 digit state slug buddy")
    }
    
    url <-
      glue::glue('https://www.realtor.com/median_prices?city={city_slug}&state_code={state}') %>%
      as.character()
    
    data <-
      df_loc_slug %>%
      mutate(urlAPI = url) %>%
      select(locationSearch, everything())
    
    data
    
  }

.generate_market_urls <-
  function(locations = c("Bethesda, MD")) {
    .generate_market_url_safe <-
      purrr::possibly(.generate_market_url, data_frame())
    locations %>%
      map_df(function(location_name) {
        .generate_market_url_safe(location_name = location_name)
      })
  }

.parse_market_data_url <-
  function(url = "https://www.realtor.com/median_prices?city=Bethesda&state_code=MD") {
    data <-
      url %>%
      .curl_json() %>%
      jsonlite::fromJSON(
        simplifyVector = T,
        simplifyDataFrame = T,
        flatten = T
      )
    
    data <-
      data$data %>%
      flatten_df() %>%
      dplyr::select(-matches("_display"))
    
    df_names <- dictionary_realtor_names()
    actual_names <-
      names(data) %>%
      map_chr(function(name) {
        df_row <- df_names %>% filter(nameRealtor == name)
        if (df_row %>% nrow() == 0) {
          glue::glue("Missing {name}") %>%
            message()
          return(name)
        }
        df_row %>%  pull(nameActual)
      })
    
    data <-
      data %>%
      purrr::set_names(actual_names) %>%
      mutate(urlAPI = url)
    
    
    if (data %>% tibble::has_name("pricePerSF")) {
      data <-
        data %>%
        dplyr::rename(pricePerSFMedian = pricePerSF)
    }
    
    data
  }


.parse_market_data_urls <-
  function(urls = "https://www.realtor.com/median_prices?city=Bethesda&state_code=MD",
           return_message = TRUE) {
    .parse_market_data_url_safe <-
      purrr::possibly(.parse_market_data_url, data_frame())
    urls %>%
      map_df(function(url) {
        if (return_message) {
          glue::glue("Parsing {url %>% str_replace_all('https://www.realtor.com/', '')}") %>%
            message()
        }
        .parse_market_data_url_safe(url = url)
      })
  }

#' Median market statistics
#'
#' Returns summary median market information for the specified
#' locations.
#'
#' The location names must be a city and/or neighborhood bounded
#' by commas.
#'
#' @param locations vector of location , location name must contain
#' a city name and a comma ie "Brooklyn, NY"
#' @param return_message if \code{TRUE} returns a message
#' @param ... extra parameters
#'
#' @return a \code{data_frame}
#' @export
#' @family market information
#' @examples
#' median_prices(locations = c("Greenwich, CT", "New London, CT", "Woodside, CA", "Park Slope, Brooklyn, NY"))
median_prices <-
  function(locations = NULL,
           return_message = TRUE,
           ...) {
    if (locations %>% purrr::is_null()) {
      stop("Please enter location names!!")
    }
    
    .generate_market_urls_safe <-
      purrr::possibly(.generate_market_urls, data_frame())
    
    df_urls <-
      .generate_market_urls_safe(locations = locations)
    
    if (df_urls %>% nrow() == 0) {
      "No results" %>% message()
      return(invisible())
    }
    
    .parse_market_data_urls_safe <-
      purrr::possibly(.parse_market_data_urls, data_frame())
    
    all_data <-
      .parse_market_data_urls_safe(urls = df_urls$urlAPI, return_message = return_message)
    
    if (all_data %>% nrow() == 0) {
      "No results" %>% message()
      return(invisible())
    }
    
    
    if (all_data %>% tibble::has_name("priceRentMedian")) {
      all_data <-
        all_data %>%
        mutate(pctRentYield = (priceRentMedian * 12) / priceListingMedian)
    }
    
    if (all_data %>% tibble::has_name("priceListingMedian")) {
      all_data <-
        all_data %>%
        mutate(
          areaPropertySFMedian = (priceListingMedian / pricePerSFMedian) %>% round(digits = 2)
        )
    }
    
    all_data <-
      all_data %>%
      left_join(df_urls) %>%
      select(one_of(
        c(
          'locationSearch',
          "streetOrNeighborhoodSearch",
          'citySearch',
          'stateSearch',
          'pricePerSFMedian',
          'priceRentMedian',
          'pctRentYield',
          'areaPropertySFMedian'
        )
      ),
      everything()) %>%
      suppressMessages() %>%
      suppressWarnings() %>%
      remove_na() %>%
      remove_columns()
    all_data
  }


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
      map_df(function(location_name) {
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
            message()
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
            message()
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
    
    data <-
      data %>%
      mutate(
        areaPropertySFMedian = (priceListingMedian / pricePerSFMedian) %>% round(digits = 2),
        pctRentYield = (priceRentMedian * 12) / priceListingMedian
      ) %>%
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
      map_df(function(url) {
        if (return_message) {
          glue::glue("Parsing {url %>% str_replace_all('https://www.realtor.com/', '')}") %>%
            message()
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
      "No results" %>% message()
      return(invisible())
    }
    
    .parse_market_trend_urls_safe <-
      purrr::possibly(.parse_market_trend_urls, data_frame())
    
    all_data <-
      .parse_market_trend_urls(urls = df_urls$urlAPI, return_message = return_message)
    
    all_data <-
      all_data %>%
      left_join(df_urls, by = 'urlAPI') %>%
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
      "No results" %>% message()
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
    
    all_data %>%
      remove_columns()
    
  }

# validate -----------------------------------------------------------------

.parse_validation_url <-
  function(url = "https://www.realtor.com/validate_geo?location=Easton%2C+MD&retain_secondary_facets=true&include_zip=false&search_controller=Search%3A%3APropertiesController") {
    data <-
      url %>%
      .curl_json() %>%
      jsonlite::fromJSON(flatten = T, simplifyDataFrame = T) %>%
      flatten_df() %>%
      as_data_frame()
    
    
    data <-
      data %>%
      dplyr::select(-one_of("postal_code")) %>%
      suppressWarnings()
    df_names <- dictionary_realtor_names()
    actual_names <-
      names(data) %>%
      map_chr(function(name) {
        df_row <- df_names %>% filter(nameRealtor == name)
        if (df_row %>% nrow() == 0) {
          glue::glue("Missing {name}") %>%
            message()
          return(name)
        }
        df_row %>% pull(nameActual)
      })
    
    data <-
      data %>%
      purrr::set_names(actual_names) %>%
      mutate(slugLDP = slugLDP %>% substr(2, nchar(slugLDP))) %>%
      mutate(urlAPI = url)
    
    data %>%
      .munge_realtor()
    
  }

.parse_validation_urls <-
  function(urls = "https://www.realtor.com/validate_geo?location=Easton%2C+MD&retain_secondary_facets=true&include_zip=false&search_controller=Search%3A%3APropertiesController",
           return_message = TRUE) {
    .parse_validation_url_safe <-
      purrr::possibly(.parse_validation_url, data_frame())
    urls %>%
      map_df(function(url) {
        if (return_message) {
          glue::glue("Parsing {url %>% str_replace_all('https://www.realtor.com/', '')}") %>%
            message()
        }
        .parse_validation_url_safe(url = url)
      })
  }


.generate_market_validation_url <-
  function(location_name = c("Bethesda, MD")) {
    location_name <- as.character(location_name)
    has_comma <- location_name %>% str_detect("\\,")
    if (has_comma) {
      df_loc_slug <-
        location_name %>%
        str_trim() %>%
        parse_location()
      
      city <-
        df_loc_slug$citySearch
      
      city_slug <-
        city %>% URLencode()
      
      state <-
        df_loc_slug$stateSearch
      
      if (state %>% nchar() > 4) {
        stop("Has to be the 2 digit state slug buddy")
      }
      
      location_slug <-
        glue::glue("{city_slug}_{state}")
      
      url <-
        glue::glue(
          "https://www.realtor.com/validate_geo?location={location_slug}&retain_secondary_facets=true&include_zip=false&search_controller=Search%3A%3APropertiesController"
        ) %>%
        as.character()
      
      data <-
        df_loc_slug %>%
        mutate(locationSearch = location_name) %>%
        mutate(urlAPI = url) %>%
        select(locationSearch, everything())
    } else {
      data <- parse_location(location_name = location_name)
      url <-
        glue::glue(
          "https://www.realtor.com/validate_geo?location={location_name}&retain_secondary_facets=true&include_zip=false&search_controller=Search%3A%3APropertiesController"
        ) %>%
        as.character()
      
      data <-
        data %>%
        mutate(urlAPI = url)
    }
    data
    
  }

.generate_market_validation_urls <-
  function(locations = c("Bethesda, MD", 20852)) {
    locations %>%
      map_df(function(location_name) {
        .generate_market_validation_url(location_name = location_name)
      }) %>%
      select(one_of(
        c(
          "locationSearch",
          "streetOrNeighborhoodSearch",
          "citySearch",
          "stateSearch",
          "zipcodeSearch",
          "urlAPI"
        )
      ),
      everything()) %>%
      suppressWarnings()
  }

validate_locations <-
  function(locations = NULL,
           return_message = TRUE,
           ...) {
    if (locations %>% purrr::is_null()) {
      stop("Please enter location names!!")
    }
    
    .generate_market_validation_urls_safe <-
      purrr::possibly(.generate_market_validation_urls, data_frame())
    
    df_urls <-
      .generate_market_validation_urls_safe(locations = locations)
    
    if (df_urls %>% nrow() == 0) {
      "No results" %>% message()
      return(invisible())
    }
    
    .parse_validation_urls_safe <-
      purrr::possibly(.parse_validation_urls, data_frame())
    
    all_data <-
      .parse_validation_urls_safe(urls = df_urls$urlAPI, return_message = return_message)
    
    if (all_data %>% nrow() == 0) {
      "No results" %>% message()
      return(invisible())
    }
    
    all_data <-
      all_data %>%
      left_join(df_urls %>% select(one_of(c(
        "locationSearch", "urlAPI"
      )))) %>%
      select(one_of(
        c(
          "locationSearch",
          "nameCity",
          "stateSearch",
          "zipcodeSearch"
        )
      ),
      everything()) %>%
      suppressMessages() %>%
      suppressWarnings() %>%
      remove_na()
    
    
    all_data
  }



# market_vitality ---------------------------------------------------------

.parse_market_vitality_url <-
  function(url = "https://www.realtor.com/home_page/vitality?location=Bethesda%2C+MD") {
    data <-
      url %>%
      .curl_json() %>%
      jsonlite::fromJSON(flatten = T, simplifyDataFrame = T)
    
    df_names <- dictionary_realtor_names()
    
    data_listings <- data$new_listings %>% as_data_frame()
    
    actual_names <-
      names(data_listings) %>%
      map_chr(function(name) {
        df_row <- df_names %>% filter(nameRealtor == name)
        if (df_row %>% nrow() == 0) {
          glue::glue("Missing {name}") %>%
            message()
          return(name)
        }
        df_row %>% pull(nameActual)
      })
    
    data_listings <-
      data_listings %>%
      purrr::set_names(actual_names) %>%
      mutate(
        priceListing = priceListing %>% readr::parse_number(),
        areaPropertySF = areaPropertySF %>% readr::parse_number(),
        urlListing = glue::glue("https://www.realtor.com/{slugLDP}") %>% as.character()
      ) %>%
      select(idListing, addressDisplay, statusListing,
             everything()) %>%
      remove_na() %>%
      select(-one_of("slugLDP")) %>%
      suppressMessages()
    
    data <-
      data[c(
        "for_sale_count",
        "new_listing_count",
        "open_house_count",
        "recently_sold_count",
        "foreclosure_count",
        "price_reduced_count"
      )] %>%
      as_data_frame()
    
    actual_names <-
      names(data) %>%
      map_chr(function(name) {
        df_row <- df_names %>% filter(nameRealtor == name)
        if (df_row %>% nrow() == 0) {
          glue::glue("Missing {name}") %>%
            message()
          return(name)
        }
        df_row %>% pull(nameActual)
      })
    
    data <-
      data %>%
      purrr::set_names(actual_names) %>%
      mutate(urlAPI = url,
             dataListingsRecent = list(data_listings))
    
    data
  }

.parse_market_vitality_urls <-
  function(urls =  "https://www.realtor.com/home_page/vitality?location=Bethesda%2C+MD",
           return_message = T) {
    .parse_market_vitality_url_safe <-
      purrr::possibly(.parse_market_vitality_url, data_frame())
    urls %>%
      map_df(function(url) {
        if (return_message) {
          glue::glue("Parsing {url %>% str_replace_all('https://www.realtor.com/', '')}") %>%
            message()
        }
        .parse_market_vitality_url_safe(url = url)
      })
  }


.generate_market_vitality_url <-
  function(location_name = c("Bethesda, MD")) {
    if (location_name %>% str_detect("\\,")) {
      df_loc_slug <-
        location_name %>%
        str_trim() %>%
        parse_location()
      city <-
        df_loc_slug$citySearch
      
      location_slug <-
        location_name %>%
        str_replace_all("\\,", "") %>%
        URLencode()
      state <- df_loc_slug$stateSearch
      
      if (state %>% nchar() > 4) {
        stop("Has to be the 2 digit state slug buddy")
      }
      
      url <-
        glue::glue('https://www.realtor.com/home_page/vitality?location={location_slug}') %>%
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
      
      data
    } else {
      data <- parse_location(location_name = location_name)
      url <-
        glue::glue('https://www.realtor.com/home_page/vitality?location={location_name}') %>%
        as.character()
      
      data <-
        data %>%
        mutate(urlAPI = url)
    }
    data
  }

.generate_market_vitality_urls <-
  function(locations = c("Bethesda, MD")) {
    .generate_market_vitality_url_safe <-
      purrr::possibly(.generate_market_vitality_url, data_frame())
    
    locations %>%
      map_df(function(location_name) {
        .generate_market_vitality_url_safe(location_name = location_name)
      })
  }


#' Market vitality
#'
#' Acquires meta level information
#' for specified locations as of the
#' most recent time period
#'
#' @param locations vector of location names, location name must contain
#' a city name and a comma ie "Brooklyn, NY" or a zipcode
#'
#' @param return_message if \code{TRUE} returns a message
#' @param ... extra parameters
#'
#' @return a \code{data_frame}
#' @export
#'
#' @examples
#' vitality(locations = c("La Jolla, CA", "Manhattan, NY", "Bethany, DE", "10016"))
vitality <-
  function(locations = NULL,
           return_message = TRUE,
           ...) {
    if (locations %>% purrr::is_null()) {
      stop("Please enter location names!!")
    }
    
    .generate_market_vitality_urls_safef <-
      purrr::possibly(.generate_market_vitality_urls, data_frame())
    
    df_urls <-
      .generate_market_vitality_urls_safef(locations = locations)
    
    if (df_urls %>% nrow() == 0) {
      "No results" %>% message()
      return(invisible())
    }
    
    .parse_market_vitality_urls_safe <-
      purrr::possibly(.parse_market_vitality_urls, data_frame())
    
    all_data <-
      .parse_market_vitality_urls_safe(urls = df_urls$urlAPI, return_message = return_message)
    
    if (all_data %>% nrow() == 0) {
      "No results" %>% message()
      return(invisible())
    }
    all_data <-
      all_data %>%
      left_join(df_urls %>% select(one_of(c(
        "locationSearch", "urlAPI"
      )))) %>%
      mutate(dateData = Sys.Date()) %>%
      select(one_of(
        c(
          "dateData",
          "locationSearch",
          "nameCity",
          "stateSearch",
          "zipcodeSearch"
        )
      ), everything()) %>%
      suppressMessages() %>%
      suppressWarnings() %>%
      remove_na() %>%
      remove_columns()
    
    
    all_data
  }

# api ---------------------------------------------------------------------
## https://www.realtor.com/browse_modules/homes_near_street?price=239000&postal_code=20816&median_price=1132000&type=homes_near_street&vis_id=6b0d44ae-f4d6-41f0-8feb-e6491ab43fe9&mcm_id=03714656198478469204855792545062287725&address=5301+Westbard+Cir+Apt+323&user_id=&city=Bethesda&coordinates=38.964595%2C-77.109017


.generate_address_url <-
  function(city,
           zip_code,
           address,
           type,
           median_price) {
    base_url <-
      'https://www.realtor.com/browse_modules/homes_near_street?'
  }

generate_coordinate_slug <-
  function(latitude = 38.964419 ,
           longitude = -77.113659) {
    glue::glue("{latitude},{longitude}") %>%
      as.character()
  }


.parse_realtor_api_near_url <-
  function(url = "https://www.realtor.com/browse_modules/homes_near_street?postal_code=20816vis_id=6b0d44ae-f4d6-41f0-8feb-e6491ab43fe9&mcm_id=03714656198478469204855792545062287725&city=New+York&coordinates=40.74516%2C-73.97852") {
    data <-
      url %>%
      .curl_json() %>%
      jsonlite::fromJSON(
        simplifyVector = T,
        simplifyDataFrame = T,
        flatten = T
      )
    
    df_properties <-
      data$properties %>%
      as_data_frame()
    
    df_names <- dictionary_realtor_names()
    actual_names <-
      names(df_properties) %>%
      map_chr(function(name) {
        df_row <- df_names %>% filter(nameRealtor == name)
        if (df_row %>% nrow() == 0) {
          glue::glue("Missing {name}") %>%
            message()
          return(name)
        }
        df_row %>% pull(nameActual)
      })
    
    df_properties <-
      df_properties %>%
      purrr::set_names(actual_names) %>%
      dplyr::select(-matches("remove")) %>%
      suppressMessages()
    
    df_properties <-
      df_properties %>%
      .munge_realtor() %>%
      mutate(urlAPI = url)
    
    df_properties
  }


# seach_results -----------------------------------------------------------

.calculate_pages <- function(page) {
  page %>%
    html_nodes('.pagination') %>%
    html_text() %>%
    str_split("\n") %>%
    flatten_chr() %>%
    str_trim() %>%
    readr::parse_number() %>%
    max(na.rm = T) %>%
    suppressWarnings()
}

.generate_search_urls <-
  function(url = "https://www.realtor.com/realestateandhomes-search/Bethesda_MD",
           radius = NULL) {
    page <- url %>% read_html()
    
    pages <- page %>% .calculate_pages()
    
    1:pages %>%
      map_df(function(x) {
        if (!radius %>% purrr::is_null()) {
          radius_slug <-
            glue::glue('/radius-{radius}') %>% as.character()
          hasRadius <- T
        } else {
          radius_slug <- ''
          hasRadius <- F
        }
        
        data_frame(
          idPage = x,
          hasRadius,
          urlListingPage = glue::glue("{url}/pg-{x}{radius_slug}") %>% as.character()
        )
      })
  }

.parse_search_page <-
  function(url = "https://www.realtor.com/realestateandhomes-search/Bethesda_MD/pg-3") {
    page <-
      .curl_page(url = url)
    result_nodes <-
      page %>%
      html_nodes('.js-save-alert-wrapper')
    
    parse_css_name_safe <-
      purrr::possibly(parse_css_name, data_frame())
    
    all_data <-
      seq_along(result_nodes) %>%
      map_df(function(x) {
        page_node <-
          result_nodes[[x]]
        df <-
          list(
            parse_css_name_safe(
              page = page_node,
              css = ".data-price-display",
              actual_name = "priceDisplay",
              is_numeric = F
            ),
            parse_css_name_safe(
              page = page_node,
              css = ".label-green",
              actual_name = "typeListing"
            ),
            parse_css_name_safe(
              page = page_node,
              css = ".srp-property-type",
              actual_name = "typeProperty"
            ) %>% mutate(value = value %>% str_replace_all("for Sale", "") %>% str_trim()),
            #
            parse_css_name_safe(
              page = page_node,
              css = ".font-bold",
              actual_name = "nameBrokerage"
            ),
            parse_css_name_safe(
              page = page_node,
              css = ".meta-beds",
              actual_name = "countBeds",
              is_numeric = F
            ),
            parse_css_name_safe(
              page = page_node,
              css = "li:nth-child(2) .data-value",
              actual_name = "countBaths",
              is_numeric = F
            ),
            
            parse_css_name_safe(
              page = page_node,
              css = "li:nth-child(3) .data-value",
              actual_name = "areaPropertySF",
              is_numeric = F
            ),
            
            parse_css_name_safe(
              page = page_node,
              css = "li:nth-child(3) .data-value",
              actual_name = "areaLot",
              is_numeric = F
            )
          ) %>%
          purrr::reduce(bind_rows) %>%
          mutate(value = value %>% str_to_title()) %>%
          spread(nameActual, value)
        
        
        property_address <-
          page_node %>% html_nodes('.js-srp-listing-photos') %>% html_attr("alt") %>%
          .[[1]]
        
        url_image <-
          page_node %>% html_nodes('.js-srp-listing-photos') %>% html_attr('src') %>% .[[1]]
        
        
        url_listing <-
          page_node %>%
          html_nodes('.srp-item-photo a') %>%
          html_attr('href') %>%
          str_c("https://www.realtor.com", .)
        
        df <-
          df %>%
          .munge_realtor() %>%
          suppressMessages() %>%
          suppressWarnings()
        
        df %>%
          mutate(
            numberListingPage = x,
            addressProperty = property_address,
            urlListing = url_listing,
            urlImage = url_image
          ) %>%
          select(numberListingPage, addressProperty, everything())
        
      }) %>%
      mutate(urlListingPage = url)
    parse_address_safe <-
      purrr::possibly(parse_address, data_frame())
    df_address <-
      all_data$addressProperty %>%
      map_df(function(address) {
        parse_address_safe(address = address)
      })
    
    all_data <-
      all_data %>%
      left_join(df_address) %>%
      select(numberListingPage,
             one_of(names(df_address)),
             everything()) %>%
      suppressMessages()
    
    all_data
  }

.parse_search_pages <-
  function(urls = "https://www.realtor.com/realestateandhomes-search/Bethesda_MD/pg-10",
           return_message = TRUE) {
    .parse_search_page_safe <-
      purrr::possibly(.parse_search_page, data_frame())
    
    urls %>%
      map_df(function(url) {
        if (return_message) {
          glue::glue("Parsing {url %>% str_replace_all('https://www.realtor.com/', '')}") %>%
            message()
        }
        .parse_search_page_safe(url = url)
      })
  }

location_listings <-
  function(location_name = c("Bethesda, MD"),
           include_features = F,
           radius = NULL,
           parse_property_details = F,
           return_message = T) {
    df_val <- validate_locations(locations = location_name)
    
    df_urls <-
      .generate_search_urls(url = df_val$urlListing, radius = radius)
    
    df_urls$urlListingPage[[1]]  <-
      df_urls$urlListingPage[[1]] %>% str_replace_all("\\/pg-1", "")
    
    all_data <-
      .parse_search_pages(urls = df_urls$urlListingPage)
    
    all_data <-
      all_data %>%
      left_join(df_urls) %>%
      select(-one_of("hasRadius")) %>%
      select(idPage, everything()) %>%
      suppressMessages() %>%
      arrange(idPage)
    
    if (parse_property_details) {
      "Parsing indvidual property listings" %>% message()
      
      parse_listing_urls_safe <-
        purrr::possibly(parse_listing_urls, data_frame())
      
      urls <-
        all_data$urlListing
      
      all_listing <-
        parse_listing_urls_safe(
          urls = urls,
          include_features = include_features,
          return_message = return_message
        )
      
      all_data <-
        all_data %>%
        left_join(all_listing) %>%
        suppressMessages()
    }
    all_data %>%
      .munge_realtor() %>%
      remove_columns()
  }

#' Property listing table
#'
#' Returns listing table.  Slower and not advised
#' unless \link{listings()} does not work for your search.
#'
#' @param locations vector of table listings
#' @param include_features if \code{TRUE} includes property featues from pages
#' @param radius if not \code{NULL} additional search radius
#' @param parse_property_details if \code{TRUE}
#' @param return_message if \code{TRUE} returns messages
#' @param ... extra parameters
#'
#' @return a \code{data_frame}
#' @export
#'
#' @examples
#' table_listings(locations = "Marietta, GA")

table_listings <-
  function(locations,
           include_features = F,
           radius = NULL,
           parse_property_details = F,
           return_message = T,
           ...) {
    location_listings_safe <-
      purrr::possibly(location_listings, data_frame())
    
    all_data <-
      locations %>%
      map_df(function(location) {
        location_listings_safe(
          location_name = location,
          include_features = include_features,
          radius = radius,
          parse_property_details = parse_property_details,
          return_message = return_message,
          ...
        )
      })
    all_data
  }


# listings ----------------------------------------------------------------

.parse_listing_url <-
  function(url = "https://www.realtor.com/realestateandhomes-detail/5301-Westbard-Cir-Apt-323_Bethesda_MD_20816_M63437-59115",
           include_features = T,
           sleep_time = NULL) {
    page <-
      .curl_page(url = url)
    
    fact_nodes <-
      page %>%
      html_nodes(".ldp-key-fact-item")
    
    data <-
      seq_along(fact_nodes) %>%
      map_df(function(x) {
        fact_node <-
          fact_nodes[[x]]
        id <-
          fact_node %>% html_node('i') %>%
          html_attr('class')
        value <-
          fact_node %>%
          html_node('.ellipsis') %>%
          html_attr('title')
        
        if (value %>% is.na()) {
          div_text <-
            fact_node %>% html_nodes("div") %>% html_text()
          if (x %in%  c(3, 5)) {
            value <-
              readr::parse_number(div_text[[2]])
          }
        }
        
        data_frame(numberItem = x,
                   id,
                   value = value %>% as.character())
        
      })
    
    data <-
      data %>%
      left_join(dictionary_css_page()) %>%
      select(-c(numberItem, id)) %>%
      spread(nameActual, value) %>%
      .munge_realtor() %>%
      suppressMessages()
    
    photo_urls <-
      page %>% html_nodes('#ldpPhotoshero img') %>% html_attr('data-src')
    
    if (photo_urls %>% length() > 0) {
      photo_urls <- photo_urls[!photo_urls %>% is.na()]
      photo_urls <-
        photo_urls[!photo_urls %>% str_detect("w60_h60")]
      
      if (photo_urls %>% length() > 0) {
        data <-
          data %>%
          mutate(urlImage = photo_urls %>% sample(1),
                 dataPhotos = list(data_frame(urlPhotoProperty = photo_urls)))
      }
    }
    
    
    address_nodes <-
      page %>%
      html_nodes('#ldp-address span')
    
    if (address_nodes %>% length() > 0) {
      df_address <-
        1:length(address_nodes) %>%
        map_df(function(x) {
          attributes <-
            address_nodes[[x]] %>% html_attrs()
          
          value <-
            address_nodes[x] %>% html_text() %>% as.character() %>% str_trim()
          
          df <-
            data_frame(item = attributes %>% names(),
                       id = attributes %>% as.character(),
                       value) %>%
            mutate(idRow = x)
          df
        }) %>%
        filter(!item == 'class') %>%
        filter(!value == '') %>%
        left_join(dictionary_css_page()) %>%
        mutate(value = value %>% str_replace_all("\\,", "")) %>%
        select(nameActual, value) %>%
        spread(nameActual, value) %>%
        suppressMessages()
      
      data <-
        data %>%
        mutate(id = 1) %>%
        left_join(df_address %>% mutate(id = 1)) %>%
        select(-id) %>%
        suppressMessages()
      
    }
    
    other_agent <- page %>% html_nodes('#ldp-branding .font-bold')
    
    if (other_agent %>% length() > 0) {
      df_broker_1 <-
        1:length(other_agent) %>%
        map_df(function(x) {
          attributes <-
            other_agent[[x]] %>% html_attrs()
          
          value <-
            other_agent[x] %>% html_text() %>% as.character()
          
          df <-
            data_frame(item = attributes %>% names(),
                       id = attributes %>% as.character(),
                       value) %>%
            mutate(idRow = x) %>%
            filter(item == 'data-label')
          df
        }) %>%
        left_join(dictionary_css_page()) %>%
        select(nameActual, value) %>%
        distinct() %>%
        spread(nameActual, value) %>%
        .munge_realtor() %>%
        suppressMessages()
      
      data <-
        data %>%
        mutate(id = 1) %>%
        left_join(df_broker_1 %>% mutate(id = 1)) %>%
        select(-id) %>%
        suppressMessages()
      
    }
    listing <-
      page %>% html_nodes('#ldp-pricewrap span') %>% html_text()
    if (listing %>% length() > 0) {
      data <-
        data %>%
        mutate(priceListing = listing)
    }
    
    listing <-
      page %>% html_nodes('#ldp-pricewrap span') %>% html_text()
    if (listing %>% length() > 0) {
      data <-
        data %>%
        mutate(priceListing = listing)
    }
    
    hood <- page %>% html_nodes('#local-name') %>% html_text()
    if (hood %>% length() > 0) {
      data <-
        data %>%
        mutate(nameNeighborhoodProperty = hood)
    }
    descriptionText <-
      page %>% html_nodes("#ldp-detail-romance") %>% html_text()
    
    feature_nodes <-
      page %>% html_nodes('.title-subsection-sm+ .row li')
    
    if (descriptionText %>% length() > 0) {
      data <-
        data %>%
        mutate(descriptionText = descriptionText %>% str_to_title())
    }
    
    
    
    broker_name_contact <-
      page %>% html_nodes('.ellipsis.link-secondary span')
    
    if (broker_name_contact %>% length() > 0) {
      values <-
        broker_name_contact  %>% html_text() %>% str_trim()
      
      df_agent <-
        1:length(broker_name_contact) %>%
        map_df(function(x) {
          attributes <- broker_name_contact[[x]] %>% html_attrs()
          df <-
            data_frame(item = attributes %>% names(),
                       id = attributes %>% as.character()) %>%
            mutate(idRow = x)
          
          if (nrow(df) >= 2) {
            df <-
              df %>%
              filter(item == "data-label")
            return(df)
          }
          
          df %>%
            filter(item == "class")
        }) %>%
        mutate(value = values) %>%
        left_join(dictionary_css_page()) %>%
        select(nameActual, value) %>%
        filter(!is.na(nameActual)) %>%
        spread(nameActual, value) %>%
        suppressMessages()
      
      data <-
        data %>%
        mutate(id = 1) %>%
        left_join(df_agent %>% mutate(id = 1)) %>%
        select(-id) %>%
        suppressMessages()
    }
    
    brokerage_name_contact <-
      page %>% html_nodes('.link-secondary .ellipsis span')
    
    
    if (brokerage_name_contact %>% length() > 0) {
      values <-
        brokerage_name_contact  %>% html_text() %>% str_trim()
      
      df_broker <-
        1:length(brokerage_name_contact) %>%
        map_df(function(x) {
          attributes <-
            brokerage_name_contact[[x]] %>% html_attrs()
          
          df <-
            data_frame(item = attributes %>% names(),
                       id = attributes %>% as.character()) %>%
            mutate(idRow = x)
          
          df <-
            df %>%
            filter(item == "data-label")
          df
        }) %>%
        mutate(value = values) %>%
        left_join(dictionary_css_page()) %>%
        select(nameActual, value) %>%
        filter(!is.na(nameActual)) %>%
        spread(nameActual, value) %>%
        suppressMessages()
      
      data <-
        data %>%
        mutate(id = 1) %>%
        left_join(df_broker %>% mutate(id = 1)) %>%
        select(-id) %>%
        suppressMessages()
    }
    
    
    
    
    if (feature_nodes %>% length() > 0 && include_features) {
      features <- feature_nodes %>% html_text()
      data <-
        data %>%
        mutate(listFeatures = list(features))
    }
    
    url_vr <-
      page %>% html_nodes('#hero-view-tour') %>% html_attr('href')
    
    if (url_vr %>% length() > 0) {
      data <-
        data %>%
        mutate(urlVRTour = url_vr)
      
    }
    
    property_meta <-
      page %>%
      html_nodes('#ldp-property-meta li')
    
    if (property_meta %>% length() > 0) {
      df_property_df <-
        1:length(property_meta) %>%
        map_df(function(x) {
          attributes <-
            property_meta[[x]] %>% html_attrs()
          
          value <-
            property_meta[x] %>% html_text() %>% as.character() %>% readr::parse_number() %>% as.character()
          
          df <-
            data_frame(item = attributes %>% names(),
                       id = attributes %>% as.character(),
                       value) %>%
            mutate(idRow = x)
          df
        }) %>%
        left_join(dictionary_css_page()) %>%
        select(nameActual, value) %>%
        distinct() %>%
        spread(nameActual, value) %>%
        .munge_realtor() %>%
        suppressMessages()
      
      data <-
        data %>%
        mutate(id = 1) %>%
        left_join(df_property_df %>% mutate(id = 1)) %>%
        select(-id) %>%
        suppressMessages()
      
    }
    
    url3d <-
      page %>% html_nodes('.ldp-matterport-label .margin-right-sm') %>% html_attr('href')
    
    if (url3d %>% length() > 0) {
      data <-
        data %>%
        mutate(url3DTour = url3d)
      
    }
    
    data <-
      data %>%
      mutate(urlListing = url)
    
    if (!sleep_time %>% purrr::is_null()) {
      Sys.sleep(time = sleep_time)
    }
    
    data
    
  }

#' Parse listing urls
#'
#' Parses a vector of listing urls
#'
#' @param urls vector of urls
#' @param include_features if \code{TRUE} includes features
#' @param return_message if \code{TRUE} returns a message
#' @param sleep_time sleep time in between url
#'
#' @return a \code{data_frame}
#' @export
#'
#' @examples
parse_listing_urls <-
  function(urls = NULL,
           include_features = F,
           sleep_time = 1,
           return_message = TRUE) {
    .parse_listing_url_safe <-
      purrr::possibly(.parse_listing_url, data_frame())
    all_data <-
      urls %>%
      map_df(function(url) {
        if (return_message) {
          glue::glue("Parsing {url %>% str_replace_all('https://www.realtor.com/', '')}") %>%
            message()
        }
        .parse_listing_url_safe(url = url,
                                include_features = include_features,
                                sleep_time = sleep_time)
      })
    
    all_data %>%
      mutat(dateData = Sys.Date()) %>%
      select(dateData, everything()) %>%
      .munge_realtor()
  }
