# gdeltr2::load_needed_packages(c("jsonlite", "purrr", "tidyr", "glue", "stringr", "curl", "dplyr", "rvest", 'lubridate', "requestsR", "stringi"))

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
    data <-
      data %>% 
      mutate_if(is.character,
                funs(ifelse(. == "", NA_character_, .))) %>% 
      dplyr::select(which(colMeans(is.na(.)) < 1))
    
    num_names <-
      data %>% 
      dplyr::select(
        dplyr::matches(
          "^area|^count[A-Z]|^price|^latitude|^longitude|^year|^index|^id[A-Z]|^number[A-Z]|^size"
        )
      ) %>%
      dplyr::select(-dplyr::matches("countyProperty")) %>%
      names()
    
    
    data <- data %>% select(-dplyr::matches("remove"))
    
    
    if (num_names %>% length() > 0) {
      data <-
        data %>%
        mutate_at(num_names,
                  funs(. %>% as.character() %>% readr::parse_number())) %>%
        suppressWarnings() %>% suppressMessages()
      
    }
    
    log_names <-
      data %>% dplyr::select(dplyr::matches("^is|^has")) %>% names()
    
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
        mutate(pricePerSFListing = 
                 case_when( areaPropertySF > 0 ~ 
                              (priceDisplay / areaPropertySF) %>% round(digits = 2),
                            TRUE ~ NA_real_
               ))
    }
    
    if (data %>% tibble::has_name("areaPropertySF") &&
        data %>%  tibble::has_name("priceListing")) {
      data <-
        data %>%
        mutate(pricePerSFListing =
                 case_when(
                   areaPropertySF > 0 ~  (priceListing / areaPropertySF) %>% round(digits = 2)
                   ,
                   TRUE ~ NA_real_
                 ))
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
    
    date_names <-
      names(data)[names(data) %>% str_detect("date[A-Z]")]
    
    date_names <- date_names[!date_names %>% str_detect("datetime")]
    
    if (date_names %>% length() > 0 ) {
      data <- data %>% 
        mutate_at(date_names, 
                  funs(. %>% mdy()))
    }
    
    datetime_names <- 
      data %>% dplyr::select(dplyr::matches("^datetime")) %>% names()
    
    
    if (datetime_names %>% length() > 0 ) {
      data <- 
        data %>% 
        mutate_at(datetime_names, funs(. %>% ymd_hms))
    }
  
    data
  }

# dict --------------------------------------------------------------------
dictionary_css_page <-
  function() {
    tibble(
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
        "property-baths-count",
        "data-featured", "data-type", "data-display_tcpa_message", 
        "name", "description", "priceCurrency", "price", "category", 
        "address", "startDate", "endDate",
        "data_source",
        "is_senior",
        "is_featured",
        "is_mls",
        "is_cozy",
        "groupBeds"
        
        
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
        "idRealtor",
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
        "countBaths",
        "isFeatured", "typeProperty", "hasMessageTCPA", 
        "nameListing", "descriptionListing", "currencyListing", "priceListing", "categoryListing", 
        "addressListing", "datetimeListingStart", "datetimeListingEnd",
        "sourceData",
        "isSeniorHome",
        "isFeatured",
        "isMLSListing",
        "isCozy",
        "groupBeds"
      )
    )
  }

dictionary_realtor_names <-
  function() {
    tibble(
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
        'is_short_sale',
        "school",
        "sold_date",
        "days_on_market",
        "form_type",
        "lead_type",
        "show_contact_a_lender",
        "show_veterans_united",
        "sold_price_to_listing_price_diff",
        "sold_price_difference_summary.is_lower",
        "sold_price_difference_summary.price_display",
        "form_type", "lead_type", "show_contact_a_lender", "show_veterans_united",
        "has_private_showing", "is_pre_approved", "is_office_standard", 
        "is_mortgage_feature", "should_suppress_ads", "show_porch_link", 
        "is_new_construction", "should_suppress_listing_agent", "is_fios_supported", 
        "is_fios_gige", "is_fios_sfu", "is_tcpa_message_enabled",
        "agent_name", "broker_name", "broker_location", "broker_number", 
        "broker_link", "mls_id", "mls_name", "mls_copy_right", "mls_abbreviation", 
        "office_contact",
        "plan_id",
        "long",
        "street",
        "street_suffix",
        "description",
        "type",
        "type_display",
        "lot_size",
        "lot_size_units",
        "lot_size_units_display",
        "permalink",
        "beds_display",
        "listed_date",
        "listed_date_display",
        "listing_age",
        "price_per_sqft_display",
        "year_built",
        "geo_slug",
        "photo_count",
        "advertiser_id",
        "neighborhood",
        "virtual_tour_url",
        "products",
        "status_display",
        "product_name",
        "schools_info",
        "active_status",
        "property_status_display",
        "office_advertiser_id",
        "property_status",
        "hoa_fee",
        "mapUrl",
        "nameStateProperty",
        "garage", "agent_advertiser_id","broker_advertiser_id",
        "stories", "list_date", "prop_type", "lot_size_display",
        "matterport_url",
        "street_direction",
        "id", "rental_estimate",
        "house_size",
        "heating",
        "cooling",
        "date_updated",
        "sold_times",
        "sold_times_text",
        "history_length",
        "history_length_text",
        "href",
        "agent_photo",
        "agent_profile_link",
        "rental_estimate_moderate",
        "value_per_bedroom_moderate",
        "closing_price", "listing_price_sqft", "listing_price", "age_days", 
        "rental_listing_price",
        "for_sale", "sold", "rental",
        "data_source",
        "is_senior",
        "is_featured",
        "is_mls",
        "is_cozy",
        "groupBeds",
        "source",
        "name",
        "listing_status",
        "toll_free_number",
        "price_min",
        "price_max",
        "beds_min",
        "beds_max",
        "baths_min",
        "baths_max",
        "baths_display",
        "sqft_min",
        "sqft_max",
        "community_id",
        "save_aware",
        "comment",
        "deposit",
        "deposit_display",
        "availability_text",
        "available_date",
        "section",
        "area",
        "area_type",
        "local_url",
        "median_listing_price_display",
        "median_sales_price_display",
        "median_rent_price_display",
        "median_sales_price",
        "median_days_on_market",
        "management_name",
        "community_name",
        "prop_status",
        "lower", "txt", "yearData", "rental_estimate_display"
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
        "areaPropertySFDisplay",
        "remove_priceListingDisplay",
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
        'isShortSale',
        "nameSchool",
        "dateSold",
        "countDaysOnMarket",
        "typeForm",
        "typeLead",
        "hasLenderContact",
        "hasVeternsUnited",
        "amountDifferenceSoldToListing",
        "isSalePriceLowerThanListing",
        "amountDifferenceSoldToListingDisplay",
        "typeForm", "typeLead", "hasLenderContact", "hasUnitedVeterans",
        "hasPrivateShowings", "isPreApproved", "isOfficeStandard", 
        "isMorgageFeatured", "hasAdsSuppressed", "hasPorchLink", 
        "isNewConstruction", "hasListingAgentSuppressed", "isFiosSupported", 
        "isFiosGIGE", "isFiosSFU", "hasMessageTCPA",
        "nameAgent", "nameBrokerage", "locationBrokerage", "telephoneBrokerage", 
        "urlBrokerage", "idMLS", "nameMLS", "copyrightMLS", "slugMLS", 
        "telephoneOffice",
        
        "idPlan",
        "longitudeProperty",
        "numberStreet",
        "suffixStreet",
        "descriptionListing",
        "remove_typeListing",
        "typeUnit",
        "areaLotSF",
        "typeUnitsLot",
        "remove_lot_size_units_display",
        "slugLDP",
        "remove_beds_display",
        "datetimeListed",
        "remove_listed_date_display",
        "countDaysListed",
        "remove_price_per_sqft_display",
        "yearBuilt",
        "slugGEO",
        "countPhotos",
        "idAdvertister",
        "nameNeighborhood",
        "urlVirtualTour",
        "remove_products",
        "remove_status_display",
        "nameProduce",
        "descriptionSchool",
        "statusActive",
        "remove_property_status_display",
        "idAdvertiserOffice",
        "statusProperty",
        "amountHOA",
        "urlGoogleMap",
        "nameStateProperty",
        "hasGarage", "idAgentAdvertiser","idBrokerageAdvertiser",
        "countStories", "datetimeListed", "typeProperty", "remove_lot_size_display",
        "urlMatterport",
        "directionStreet",
        "idProperty", "amountRentEstimated",
        "typeSizeHouse",
        "typeHeating",
        "typeCooling",
        "dateUpdated",
        "countSales",
        "remove_sales",
        "lengthHistory",
        "remove_history",
        "urlPhotoPrimary",
        "urlAgentPhoto",
        "slugAgentProfile" ,
        "rentEstimatedMonthlyModerate",
        "rentPerBedroomMonthlyModerate",
        "priceClosing", "priceListingPSF", "priceListing", 
        "countDaysListing", 
        "rentListing",
        "countSaleListings", 
        "countSoldListings",
        "countRentalListings",
        "sourceData",
        "isSeniorHome",
        "isFeatured",
        "isMLSListing",
        "isCozy",
        "groupBeds",
        "sourceListng",
        "nameListing",
        "statusListing",
        "telephoneListing",
        "priceListingMin",
        "priceListingMax",
        "countBedsMin",
        "countBedsMax",
        "countBathsMin",
        "countBathsMax",
        "bathsDisplay",
        "areaPropertySFMin",
        "areaPropertySFMax",
        "idCommunity",
        "isSaveAware",
        "commentListing",
        "depositDescription",
        "depositDescriptionDisplay",
        "descriptionAvailability",
        "dateAvailable",
        "sectionSite",
        "nameArea",
        "typeArea",
        "slugRealtorURL",
        "remove_median_listing_price_display",
        "remove_median_sales_price_display",
        "priceRentMedian",
        "priceSalesMedian",
        "countDaysOnMarketMedian",
        "nameManagementCompany",
        "nameCommunity",
        "statusProperty",
        "isLowerEstimate", "displayValueEstimate", "yearData", "displayRentalEstimate"
      )
    )
  }


# rates -------------------------------------------------------------------

# https://www.realtor.com/mrtg_handler/get_trends_data

#' Mortgage Rates
#'
#' Returns a variety of
#' interest rates for various
#' mortgage types.
#'
#' @param return_wide if \code{TRUE} widens data and removes duration and benchmark variables
#'
#' @return a \code{tibble}
#' @export
#' @family interest rates
#' @examples
#' mortgage_rates(return_wide = F)
mortgage_rates <-
  function(return_wide = F) {
    data <-
      "https://www.realtor.com/mrtg_handler/get_trends_data" %>%
      jsonlite::fromJSON(flatten = T, simplifyDataFrame = T) %>%
      .$rate_trends %>%
      dplyr::as_tibble()
    
    df_types <-
      tibble(
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
          'pct30YearFixedFHA',
          'pct30YearFixedVA',
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
          tibble(
            locationSearch = location_name,
            citySearch = location_slugs[[1]],
            stateSearch = location_slugs[[2]]
          )
        return(data)
        
      } else {
        location_slugs <-
          location_name %>% str_split("\\,") %>% flatten_chr() %>% str_trim()
        
        data <-
          tibble(
            locationSearch = location_name,
            streetOrNeighborhoodSearch = location_slugs[[1]],
            citySearch = location_slugs[[2]],
            stateSearch = location_slugs[[3]]
          )
        
        return(data)
      }
    }
    
    tibble(locationSearch = location_name, zipcodeSearch = location_name)
    
    
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
      purrr::possibly(.generate_market_url, tibble())
    locations %>%
      future_map_dfr(function(location_name) {
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
      dplyr::select(-dplyr::matches("_display"))
    
    df_names <- dictionary_realtor_names()
    actual_names <-
      names(data) %>%
      map_chr(function(name) {
        df_row <- df_names %>% filter(nameRealtor == name)
        if (df_row %>% nrow() == 0) {
          glue::glue("Missing {name}") %>%
            cat(fill = T)
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
      purrr::possibly(.parse_market_data_url, tibble())
    urls %>%
      future_map_dfr(function(url) {
        if (return_message) {
          glue::glue("Parsing {url %>% str_replace_all('https://www.realtor.com/', '')}") %>%
            cat(fill = T)
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
#' @return a \code{tibble}
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
      purrr::possibly(.generate_market_urls, tibble())
    
    df_urls <-
      .generate_market_urls_safe(locations = locations)
    
    if (df_urls %>% nrow() == 0) {
      "No results" %>% cat(fill = T)
      return(invisible())
    }
    
    .parse_market_data_urls_safe <-
      purrr::possibly(.parse_market_data_urls, tibble())
    
    all_data <-
      .parse_market_data_urls_safe(urls = df_urls$urlAPI, return_message = return_message)
    
    if (all_data %>% nrow() == 0) {
      "No results" %>% cat(fill = T)
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



# validate -----------------------------------------------------------------

.parse_validation_url <-
  function(url = "https://www.realtor.com/validate_geo?location=Easton%2C+MD&retain_secondary_facets=true&include_zip=false&search_controller=Search%3A%3APropertiesController") {
    data <-
      url %>%
      .curl_json() %>%
      jsonlite::fromJSON(flatten = T, simplifyDataFrame = T) %>%
      flatten_df() %>%
      as_tibble()
    
    
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
            cat(fill = T)
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
      purrr::possibly(.parse_validation_url, tibble())
    urls %>%
      future_map_dfr(function(url) {
        if (return_message) {
          glue::glue("Parsing {url %>% str_replace_all('https://www.realtor.com/', '')}") %>%
            cat(fill = T)
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
      
      
      if (df_loc_slug %>% tibble::has_name("streetOrNeighborhoodSearch")) {
        street_slug <- 
          df_loc_slug$streetOrNeighborhoodSearch %>% 
          str_replace_all("\\ ", "%20")
        street_slug <- street_slug %>% str_c("_")
      } else {
        street_slug <- ''
      }
      
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
        glue::glue("{street_slug}{city_slug}_{state}")
      
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
      future_map_dfr(function(location_name) {
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
      purrr::possibly(.generate_market_validation_urls, tibble())
    
    df_urls <-
      .generate_market_validation_urls_safe(locations = locations)
    
    if (df_urls %>% nrow() == 0) {
      "No results" %>% cat(fill = T)
      return(invisible())
    }
    
    .parse_validation_urls_safe <-
      purrr::possibly(.parse_validation_urls, tibble())
    
    all_data <-
      .parse_validation_urls_safe(urls = df_urls$urlAPI, return_message = return_message)
    
    if (all_data %>% nrow() == 0) {
      "No results" %>% cat(fill = T)
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
    json_data <-
      url %>%
      .curl_json() %>%
      jsonlite::fromJSON(flatten = T, simplifyDataFrame = T)
    
    df_names <- dictionary_realtor_names()
    
    data <- json_data$data
    data_listings <-
      data$new_listings %>% 
      data.frame(stringsAsFactors = F) %>% 
      as_tibble()
    
    actual_names <-
      names(data_listings) %>%
      map_chr(function(name) {
        df_row <- df_names %>% filter(nameRealtor == name)
        if (df_row %>% nrow() == 0) {
          glue::glue("Missing {name}") %>%
            cat(fill = T)
          return(name)
        }
        df_row %>% pull(nameActual)
      })
    
    data_listings <-
      data_listings %>%
      purrr::set_names(actual_names) 
    
    if (!data_listings %>% tibble::has_name("areaPropertySF") &&
        data_listings %>% tibble::has_name("areaPropertySFDisplay")) {
      data_listings <- 
        data_listings %>% 
        rename(areaPropertySF = areaPropertySFDisplay) %>% 
        mutate(areaPropertySF = readr::parse_number(areaPropertySF))
    }
    
    
    data_listings <- 
      data_listings %>%
      mutate(
        priceListing = priceListing %>% as.character() %>% readr::parse_number(),
        areaPropertySF = areaPropertySF %>% as.character() %>% readr::parse_number(),
        urlListing = glue::glue("https://www.realtor.com/{slugLDP}") %>% as.character()
      ) %>%
      select(idListing, addressDisplay, statusListing,
             everything()) %>%
      remove_na() %>%
      select(-one_of("slugLDP")) %>%
      suppressWarnings() %>% suppressMessages()
    
    data <-
      data[c(
        "for_sale_count",
        "new_listing_count",
        "open_house_count",
        "recently_sold_count",
        "foreclosure_count",
        "price_reduced_count"
      )] %>%
      as_tibble()
    
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
      mutate(urlAPI = url,
             dataListingsRecent = list(data_listings))
    
    data
  }

.parse_market_vitality_urls <-
  function(urls =  "https://www.realtor.com/home_page/vitality?location=Bethesda%2C+MD",
           return_message = T) {
    .parse_market_vitality_url_safe <-
      purrr::possibly(.parse_market_vitality_url, tibble())
    urls %>%
      future_map_dfr(function(url) {
        if (return_message) {
          glue::glue("Parsing {url %>% str_replace_all('https://www.realtor.com/', '')}") %>%
            cat(fill = T)
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
      purrr::possibly(.generate_market_vitality_url, tibble())
    
    locations %>%
      future_map_dfr(function(location_name) {
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
#' @return a \code{tibble}
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
      purrr::possibly(.generate_market_vitality_urls, tibble())
    
    df_urls <-
      .generate_market_vitality_urls_safef(locations = locations)
    
    if (df_urls %>% nrow() == 0) {
      "No results" %>% cat(fill = T)
      return(invisible())
    }
    
    .parse_market_vitality_urls_safe <-
      purrr::possibly(.parse_market_vitality_urls, tibble())
    
    all_data <-
      .parse_market_vitality_urls_safe(urls = df_urls$urlAPI, return_message = return_message)
    
    if (all_data %>% nrow() == 0) {
      "No results" %>% cat(fill = T)
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


.generate_near_url <-
  function(latitude =  39.03665,
           longitude = -77.11804) {
    base_url <-
      'https://www.realtor.com/browse_modules/homes_near_street?'
    
    geo_slug <- 
      generate_coordinate_slug(latitude = latitude, longitude = longitude)
    
    
    url <- 
      glue::glue("{base_url}&coordinates={geo_slug}") %>% URLencode()
    
    url
    
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
      as_tibble()
    
    df_names <-
      dictionary_realtor_names()
    actual_names <-
      names(df_properties) %>%
      map_chr(function(name) {
        df_row <- df_names %>% filter(nameRealtor == name)
        if (df_row %>% nrow() == 0) {
          glue::glue("Missing {name}") %>%
            cat(fill = T)
          return(name)
        }
        df_row %>% pull(nameActual)
      })
    
    df_properties <-
      df_properties %>%
      purrr::set_names(actual_names) %>%
      dplyr::select(-dplyr::matches("remove")) %>%
      suppressMessages()
    
    df_properties <-
      df_properties %>%
      .munge_realtor() %>%
      mutate(urlAPI = url)
    
    df_properties
  }

property_near <- 
  function(location = "5836 Mossrock Drive, North Bethesda, MD",
           return_message = T) {
    df_loc <-
      geocode(locations = location,
              return_message = return_message) %>%
      filter(!is.na(latitudeLocation))    %>%
      dplyr::slice(1) %>%
      remove_na()
    
    url <-
      .generate_near_url(latitude = df_loc$latitudeLocation,
                         longitude = df_loc$longitudeLocation)
    
    df_loc <-
      df_loc %>%
      mutate(urlAPI = url)
    
    data <-
      .parse_realtor_api_near_url(url = url)
    
    data <-
      data %>%
      left_join(df_loc) %>%
      select(one_of(names(df_loc)), everything()) %>%
      select(-urlAPI) %>%
      suppressMessages() %>%
      .munge_realtor()
    
    data$urlListing <-
      data$urlListing %>% str_replace_all("https://www.realtor.com//", "https://www.realtor.com/")
    
    data
  }

#' Properties near a location
#' 
#' This function returns 50
#' properties near a specified location.
#' 
#' The location can be an exact address, zipcode
#' city or a neighborhood.
#'
#' @param locations a vector of locations
#' @param return_message if \code{TRUE} returns a message
#'
#' @return
#' @export
#'
#' @examples
#' locations <-  c("2449 Tracy Place, NW, Washington DC", "Sunset Island, Miami Beach, FL" )
#' properties_near(locations = locations)

properties_near <-
  function(locations = NULL,
           return_message = TRUE) {
    
    if (locations %>% purrr::is_null()) {
      stop("Enter locations")
    }
    property_near_safe <- 
      purrr::possibly(property_near, tibble())
    
    all_data <- 
      locations %>% 
      future_map_dfr(function(location){
        property_near_safe(location = location, 
                      return_message = return_message)
      })
    
    all_data
      
  }

# seach_results -----------------------------------------------------------

.calculate_pages <- function(page) {
  page %>%
    html_nodes('.pagination') %>%
    html_text() %>%
    str_split("\n") %>%
    flatten_chr() %>%
    str_trim() %>%
    as.character() %>% 
    readr::parse_number() %>%
    max(na.rm = T) %>%
    suppressWarnings() %>% suppressMessages()
}

.generate_search_urls <-
  function(url = "https://www.realtor.com/realestateandhomes-search/Bethesda_MD",
           radius = NULL) {
    page <- url %>% read_html()
    
    pages <- page %>% .calculate_pages()
    
    1:pages %>%
      future_map_dfr(function(x) {
        if (!radius %>% purrr::is_null()) {
          radius_slug <-
            glue::glue('/radius-{radius}') %>% as.character()
          hasRadius <- T
        } else {
          radius_slug <- ''
          hasRadius <- F
        }
        
        tibble(
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
      purrr::possibly(parse_css_name, tibble())
    
    all_data <-
      seq_along(result_nodes) %>%
      future_map_dfr(function(x) {
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
      purrr::possibly(parse_address, tibble())
    df_address <-
      all_data$addressProperty %>%
      future_map_dfr(function(address) {
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
      purrr::possibly(.parse_search_page, tibble())
    
    urls %>%
      future_map_dfr(function(url) {
        if (return_message) {
          glue::glue("Parsing {url %>% str_replace_all('https://www.realtor.com/', '')}") %>%
            cat(fill = T)
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
      "Parsing indvidual property listings" %>% cat(fill = T)
      
      parse_listing_urls_safe <-
        purrr::possibly(parse_listing_urls, tibble())
      
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
#' @param include_features if \code{TRUE} includes property features from pages
#' @param radius if not \code{NULL} additional search radius
#' @param parse_property_details if \code{TRUE}
#' @param return_message if \code{TRUE} returns messages
#' @param ... extra parameters
#'
#' @return a \code{tibble}
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
      purrr::possibly(location_listings, tibble())
    
    all_data <-
      locations %>%
      future_map_dfr(function(location) {
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
    options(warn = 0)
    page <-
      .curl_page(url = url)
    
    fact_nodes <-
      page %>%
      html_nodes(".ldp-key-fact-item")
    
    data <-
      seq_along(fact_nodes) %>%
      future_map_dfr(function(x) {
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
              readr::parse_number(as.character(div_text[[2]])) %>%
              suppressWarnings()
          }
        }
        
        tibble(numberItem = x,
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
          mutate(
            urlImage = photo_urls %>% sample(1),
            dataPhotos = list(tibble(urlPhotoProperty = photo_urls)),
            countPhotos = dataPhotos %>% map_dbl(nrow)
          )
      }
    }
    
    address_nodes <-
      page %>%
      html_nodes('#ldp-address span')
    
    if (address_nodes %>% length() > 0) {
      df_address <-
        seq_along(address_nodes) %>%
        future_map_dfr(function(x) {
          attributes <-
            address_nodes[[x]] %>% html_attrs()
          
          value <-
            address_nodes[x] %>% html_text() %>% as.character() %>% str_trim()
          
          df <-
            tibble(item = attributes %>% names(),
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
        seq_along(other_agent) %>%
        future_map_dfr(function(x) {
          attributes <-
            other_agent[[x]] %>% html_attrs()
          
          value <-
            other_agent[x] %>% html_text() %>% as.character()
          
          df <-
            tibble(item = attributes %>% names(),
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
      listing <- listing %>% as.character() %>% readr::parse_number() %>%
        suppressWarnings() %>% suppressMessages()
      listing <- listing[!listing %>% is.na()]
      data <-
        data %>%
        mutate(priceListing = listing[[1]])
    }
    
    hood_detail <-
      page %>% html_nodes("#ldp-neighborhood-section .padding-bottom")
    
    if (hood_detail %>% length() > 0) {
      hood_details <-
        hood_detail %>%
        html_nodes('a') %>% html_text() %>% str_trim()
      
      urls <-
        hood_detail %>%
        html_nodes('a') %>% html_attr('href') %>%
        str_c("https://www.realtor.com/", .)
      
      hood_stats <-
        page %>%
        html_nodes('.neighborhood-local-item p') %>%
        html_text() %>%
        as.character() %>%
        readr::parse_number() %>%
        suppressMessages() %>%
        suppressWarnings()
      
      if (length(hood_details) >= 2 && length(hood_stats) == 4) {
        df_hood <-
          tibble(
            item = c(
              "priceListingMedianNeighborhood",
              "priceSaleMedianNeighborHood",
              "countDaysOnMarketMedianNeighborhood",
              "pricePerSFSaleMedianNeighborhood"
            ),
            value = hood_stats
          ) %>%
          spread(item, value) %>%
          mutate(
            nameNeighborhood = hood_details[[1]],
            nameCity = hood_details[[2]],
            urlNeighborhood = urls[[1]],
            urlCity = urls[[2]]
          ) %>%
          select(nameCity, nameNeighborhood, everything())
        
        data <-
          data %>%
          mutate(dataNeighborhood = list(df_hood))
      }
      
      if (length(hood_details) >= 2 && length(hood_stats) == 2) {
        df_hood <-
          tibble(
            item = c(
              "priceListingMedianNeighborhood",
              "pricePerSFSaleMedianNeighborhood"
            ),
            value = hood_stats
          ) %>%
          spread(item, value) %>%
          mutate(
            nameNeighborhood = hood_details[[1]],
            nameCity = hood_details[[2]],
            urlNeighborhood = urls[[1]],
            urlCity = urls[[2]]
          ) %>%
          select(nameCity, nameNeighborhood, everything())
        
        data <-
          data %>%
          mutate(dataNeighborhood = list(df_hood))
      }
      
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
      
      values <- values[!values %>% str_to_lower() == "by"]
      
      df_agent <-
        seq_along(broker_name_contact) %>%
        future_map_dfr(function(x) {
          attributes <- broker_name_contact[[x]] %>% html_attrs()
          df <-
            tibble(item = attributes %>% names(),
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
        seq_along(brokerage_name_contact) %>%
        future_map_dfr(function(x) {
          attributes <-
            brokerage_name_contact[[x]] %>% html_attrs()
          
          df <-
            tibble(item = attributes %>% names(),
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
    
    if (feature_nodes %>% length() > 0 &&
        include_features) {
      features <- feature_nodes %>% html_text()
      df_features <- tibble(descriptionFeature = features)
      data <-
        data %>%
        mutate(dataFeatures = list(df_features))
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
        seq_along(property_meta) %>%
        future_map_dfr(function(x) {
          attributes <-
            property_meta[[x]] %>% html_attrs()
          
          value <-
            property_meta[x] %>% html_text() %>% as.character() %>% readr::parse_number() %>%
            suppressWarnings() %>% suppressMessages() %>%
            as.character()
          
          df <-
            tibble(item = attributes %>% names(),
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
    
    listing_history <-
      page %>% html_nodes("#ldp-history-price td") %>% html_text()
    
    if (listing_history %>% length() > 0) {
      times <- length(listing_history) %/% 3
      items <-
        rep(c("dateListing", "descriptionEvent", "priceListing"), times)
      
      df_listings <-
        tibble(item = items, value = listing_history) %>%
        group_by(item) %>%
        mutate(idEvent = 1:n()) %>%
        ungroup() %>%
        spread(item, value) %>%
        mutate(
          dateListing = dateListing %>% lubridate::mdy(),
          priceListing = priceListing %>% as.character() %>% readr::parse_number()
        ) %>%
        arrange(dateListing) %>%
        mutate(numberListing = 1:n()) %>%
        select(-idEvent) %>%
        select(numberListing, everything()) %>%
        suppressWarnings() %>% suppressMessages()
      
      data <-
        data %>%
        mutate(dataListingHistory = list(df_listings)) %>%
        mutate(countListings = dataListingHistory %>% map_dbl(nrow))
      
    }
    
    comps <-
      page %>% html_nodes('.col-xxs-3') %>% html_text() %>% str_trim()
    
    if (comps %>% length > 0) {
      if (nchar(comps) > 20) {
        homes <-
          page %>% html_nodes("#ldp-home-values .ellipsis") %>% html_text() %>% str_trim() %>% str_split("\\:|\\,")
        
        addresses <- seq_along(homes) %>%
          map_chr(function(x) {
            homes[[x]] %>% str_trim() %>% str_split("\\ ") %>%  flatten_chr() %>% discard( ~
                                                                                             .x == "") %>%
              str_c(collapse = " ") %>% str_replace_all("\\This Home ", "")
          })
        
        price_estimate <-
          page %>% html_nodes('.col-xxs-3') %>% html_text()
        
        
        start <- length(price_estimate) -  length(addresses) + 1
        
        price_estimate <-
          price_estimate[start:length(price_estimate)]  %>% as.character() %>% readr::parse_number() %>% suppressWarnings() %>%
          suppressMessages()
        
        area_sf <-
          page %>% html_nodes('.col-sm-1') %>% html_text() %>% as.character() %>% readr::parse_number() %>%
          suppressWarnings() %>% suppressMessages()
        
        
        df_comps <-
          tibble(addresseComp = addresses,
                     priceEstimate = price_estimate)
        
        if (area_sf %>% length() - 1 == nrow(df_comps)) {
          area_sf <-
            area_sf[2:length(area_sf)] %>% as.character() %>% readr::parse_number() %>%
            suppressWarnings() %>% suppressMessages()
          
          df_comps <-
            df_comps %>%
            mutate(areaPropertySF = area_sf)
        }
        
        if ((area_sf %>% length() - 3)  %/% 3 == nrow(df_comps)) {
          area_sf <-
            area_sf[4:length(area_sf)]
          items <-
            rep(c("countBeds", "countBathrooms", "areaPropertySF"),
                times = nrow(df_comps))
          
          items <-
            rep(c("countBeds", "countBathrooms", "areaPropertySF"),
                times = nrow(df_comps))
          df_bed_bath <-
            tibble(
              idItem = rep(1:3, times = nrow(df_comps)),
              item = items[seq_along(area_sf)],
              value = area_sf
            ) %>%
            group_by(item) %>%
            mutate(numberProperty = 1:n()) %>%
            select(numberProperty, item, value) %>%
            ungroup() %>%
            mutate(value = value %>% as.character() %>% readr::parse_number()) %>%
            suppressWarnings() %>% suppressMessages() %>%
            spread(item, value)
          
          df_comps <-
            df_comps %>%
            mutate(numberProperty = 1:n()) %>%
            left_join(df_bed_bath) %>%
            suppressMessages() %>%
            select(-numberProperty)
          
        }
        
        links <-
          page %>% html_nodes("#ldp-home-values a") %>% html_attr('href')
        links <-
          links[!links %>% is.na()] %>% str_c("https://www.realtor.com/realestateandhomes-detail", .)
        
        listing_comps <- c(url, links)
        
        if (length(listing_comps) == nrow(df_comps)) {
          df_comps <-
            df_comps %>%
            mutate(urlListingComp = listing_comps)
        }
        
        lot_nodes <-
          page %>% html_nodes(".hidden-xxs.col-sm-2") %>% html_text()
        
        if (length(lot_nodes) - 1 == nrow(df_comps)) {
          lots <-
            lot_nodes[2:length(lot_nodes)] %>% as.character() %>%  readr::parse_number() %>%
            suppressWarnings() %>% suppressMessages()
          df_comps <-
            df_comps %>%
            mutate(areaLotSF = lots)
        }
        
        data <-
          data %>%
          mutate(dataComps = list(df_comps),
                 countComps = dataComps %>% map_dbl(nrow))
      }
      
    }
    
    taxes <-
      page %>%
      html_nodes("#ldp-history-taxes td") %>% html_text() %>% as.character() %>% readr::parse_number() %>%
      suppressWarnings() %>% suppressMessages()
    
    
    if (taxes %>% length > 0) {
      tax_data <-
        page %>%
        html_nodes("#ldp-history-taxes td") %>% html_text() %>% as.character() %>% readr::parse_number() %>%
        suppressWarnings() %>% suppressMessages()
      
      times <-
        length(tax_data) %/% 3
      items <-
        rep(c("yearTaxes", "amountTaxes", "amountTaxableValue"), times)
      
      df_taxes <- tibble(item = items, value = tax_data) %>%
        group_by(item) %>%
        mutate(idEvent = 1:n()) %>%
        ungroup() %>%
        spread(item, value) %>%
        select(yearTaxes, amountTaxes, amountTaxableValue)
      
      data <-
        data %>%
        mutate(dataTaxes = list(df_taxes))
    }
    
    
    schools <-
      page %>% html_nodes('#load-more-schools td') %>% html_text() %>% str_trim()
    
    if (schools %>% length() > 0) {
      value <-
        page %>% html_nodes('#load-more-schools td') %>% html_text() %>% str_trim()
      repeats <- length(value) %/% 2
      item <-
        rep(c("ratingSchool", "nameSchool"), repeats)
      df_school <-
        tibble(item, value) %>%
        group_by(item) %>%
        mutate(numberSchool = 1:n()) %>%
        ungroup() %>%
        spread(item, value) %>%
        mutate_at('ratingSchool',
                  funs(. %>%  as.character() %>% readr::parse_number())) %>%
        suppressMessages() %>%
        suppressWarnings()
      
      urlSchool <-
        page %>%
        html_nodes('#load-more-schools .text-align-center+ td a') %>%
        html_attr('href') %>%
        str_c("https://www.realtor.com/local/schools/montauk-school_0751452801",
              .)
      
      df_school <-
        df_school %>%
        mutate(urlSchool)
      
      data <-
        data %>%
        mutate(dataSchool = list(df_school))
    }
    
    data <-
      data %>%
      mutate(urlListing = url)
    
    if (!sleep_time %>% purrr::is_null()) {
      Sys.sleep(time = sleep_time)
    }
    
    data
    
  }

.parse_listing_urls_html <-
  function(urls = NULL,
           include_features = F,
           sleep_time = 1,
           return_message = TRUE) {
    .parse_listing_url_safe <-
      purrr::possibly(.parse_listing_url, tibble())
    all_data <-
      urls %>%
      future_map_dfr(function(url) {
        if (return_message) {
          glue::glue("Parsing {url %>% str_replace_all('https://www.realtor.com/', '')}") %>%
            cat(fill = T)
        }
        .parse_listing_url_safe(url = url,
                                include_features = include_features,
                                sleep_time = sleep_time) %>% 
          suppressWarnings()
      })
    
    all_data <- 
      all_data %>%
      mutate(dateData = Sys.Date()) %>%
      select(dateData, everything()) %>%
      .munge_realtor()
    
    if (all_data %>% tibble::has_name("dataComps")) {
      all_data <- 
        all_data %>% 
        mutate(hasComps = dataComps %>% map_dbl(length) > 0)
    }
    
    if (all_data %>% tibble::has_name("dataTaxes")) {
      all_data <- 
        all_data %>% 
        mutate(hasTaxes = dataTaxes %>% map_dbl(length) > 0)
    }
    
    if (all_data %>% tibble::has_name("dataPhotos")) {
      all_data <- 
        all_data %>% 
        mutate(hasPhotos = dataPhotos %>% map_dbl(length) > 0)
    }
    
    if (all_data %>% tibble::has_name("dataSchool")) {
      all_data <- 
        all_data %>% 
        mutate(hasSchools = dataSchool %>% map_dbl(length) > 0)
    }
    
    if (all_data %>% tibble::has_name("dataNeighborhood")) {
      all_data <- 
        all_data %>% 
        mutate(hasNeighborhood = dataNeighborhood %>% map_dbl(length) > 0)
    }
    
    if (all_data %>% tibble::has_name("dataListingHistory")) {
      all_data <- 
        all_data %>% 
        mutate(hasListingHistory = dataListingHistory %>% map_dbl(length) > 0)
    }
    
    if (all_data %>% tibble::has_name("nameAgent")) {
      all_data <- 
        all_data %>% 
        mutate(nameAgent = nameAgent %>% str_replace_all("\\, Agent|\\, Broker", "") %>% str_trim() %>% 
                 str_to_upper())
    }
    
    if (all_data %>% tibble::has_name("statusListing")) {
      all_data <- 
        all_data %>% 
        mutate(statusListingDetail =  statusListing)
    }
    
    all_data
  }


