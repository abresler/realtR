# gdeltr2::load_needed_packages(c("jsonlite", "purrr", "tidyr", "glue", "stringr", "curl", "dplyr", "rvest"))

# munge -------------------------------------------------------------------

.munge_realtor <-
  function(data) {
    num_names <-
      data %>% dplyr::select(matches("^area|^count|^price|^latitude|^longitude|^year|^index")) %>% names()
    
    
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
    data %>% 
      remove_na()
  }

# dict --------------------------------------------------------------------
dictionary_css_page <- 
  function() {
    data_frame(id = c("ra ra-status-sale", "ra ra-price-per-sq-ft", "ra ra-days-on-realtor", 
                      "ra ra-property-type", "ra ra-year-built", "ra ra-home-style"
    ),
    nameActual = c('statusListing', 'pricePerSF', 'countDaysOnRealtor', 'typePropertyDetail',
                   'yearBuilt', 'styleHome')
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
        "year", "month", "active_listing_count", "hotmarket_rank", 
        "hotmarket_index", "hotmarket_temperature", "asking_to_sold_price_percent_change", 
        "sales_to_inventory_count_percent", "sales_to_inventory_count_percent_market_type", 
        "median.age_days", "median.listing_price", "median.rental_listing_price", 
        "median.listing_price_sqft", "median.closing_price", "listing_count.for_sale", 
        "listing_count.rental", "listing_count.sold"
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
        "areaSF",
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
        "yearData", "monthData", "countActiveListings", "rankMarketHot", 
        "indexMarketTemperature", "temperatureMarket", "pctSoldPriceToAsk", 
        "pctSalesToInventory", "typeSalesToInventoryFavor", 
        "countDaysMarketMedian", "priceListingMedian", "priceRentMedian", 
        "pricePerSFMedian", "priceClosingMedian", "countForSale", 
        "countRental", "countSold"
      )
    )
  }


# rates -------------------------------------------------------------------

# https://www.realtor.com/mrtg_handler/get_trends_data

#' Gets current mortgage rates
#'
#' @param return_wide if \code{TRUE} widens data and removes duration and benchmark variables
#'
#' @return a \code{data_frame}
#' @export
#'
#' @examples
#' get_mortage_rates(return_wide = F)
get_mortage_rates <-
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
          'pct30YeaFixedFHA',
          'pct30YearFixedFHA',
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
      location_slugs <-
        location_name %>% str_split("\\,") %>% flatten_chr() %>% str_trim()
      
      data <-
        data_frame(
          locationSearch = location_name,
          citySearch = location_slugs[[1]],
          stateSearch = location_slugs[[2]]
        )
      return(data)
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
  function(location_names = c("Bethesda, MD")) {
    .generate_market_url_safe <-
      purrr::possibly(.generate_market_url, data_frame())
    location_names %>%
      map_df(function(location_name) {
        .generate_market_url_safe(location_name = location_name)
      })
  }

.parse_market_data_url <-
  function(url = "https://www.realtor.com/median_prices?city=Bethesda&state_code=MD") {
    data <-
      url %>% jsonlite::fromJSON(
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
    df <-
      data_frame()
    
    success <- function(res) {
      url <-
        res$url
      
      if (return_message) {
        glue::glue("Parsing {url}") %>%
          message()
      }
      .parse_market_data_url_safe <-
        purrr::possibly(.parse_market_data_url , data_frame())
      
      all_data <-
        .parse_market_data_url_safe(url = url)
      
      
      df <<-
        df %>%
        bind_rows(all_data)
    }
    failure <- function(msg) {
      data_frame()
    }
    urls %>%
      map(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }

#' Get median market statistics for specified locations
#'
#' Returns summary market information for the specified
#' location.  The locaiton name must be a city bounded by a comma
#'
#' @param location_names vector of location names, location name must contain
#' a city name and a comma ie "Brooklyn, NY"
#' @param return_message if \code{TRUE} returns a message
#' @param ...
#'
#' @return a \code{data_frame}
#' @export
#'
#' @examples
#' get_median_prices(location_names = c("Greenwich, CT", "New London, CT", "Woodside, CA"))
get_median_prices <-
  function(location_names = NULL,
           return_message = TRUE,
           ...) {
    if (location_names %>% purrr::is_null()) {
      stop("Please enter location names!!")
    }
    
    .generate_market_urls_safe <-
      purrr::possibly(.generate_market_urls, data_frame())
    
    df_urls <-
      .generate_market_urls_safe(location_names = location_names)
    
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
        mutate(areaSFMedian = (priceListingMedian / pricePerSFMedian) %>% round(digits = 0))
    }
    
    all_data <-
      all_data %>%
      left_join(df_urls) %>%
      select(one_of(
        c(
          'locationSearch',
          'citySearch',
          'stateSearch',
          'pricePerSFMedian',
          'priceRentMedian',
          'pctRentYield',
          'areaSFMedian'
        )
      ),
      everything()) %>%
      suppressMessages() %>%
      suppressWarnings() %>%
      remove_na()
    all_data
  }


# market_trends -----------------------------------------------------------
.generate_market_trend_url <-
  function(location_name = c("Bethesda, MD")) {
    is_city_state <- location_name %>% str_detect("\\,")
    if (is_city_state) {
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
  function(location_names = c("Bethesda, MD")) {
    .generate_market_trend_url_safe <-
      purrr::possibly(.generate_market_trend_url, data_frame())
    
    location_names %>%
      map_df(function(location_name) {
        .generate_market_trend_url_safe(location_name = location_name)
      }) %>% 
      dplyr::select(one_of(c("locationSearch", "citySearch", "stateSearch", "zipcodeSearch", "urlAPI")), everything()) %>% 
      suppressWarnings() %>% 
      suppressMessages()
  }


.parse_market_trend_url <- 
  function(url = "https://www.realtor.com/local/markettrends/city/Marietta_GA") {
    json_data <-
      url %>%
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
        areaSFMedian = (priceListingMedian / pricePerSFMedian) %>% round(digits = 0),
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
  function(urls ="https://www.realtor.com/local/markettrends/city/Marietta_GA",
           return_message = TRUE) {
    df <-
      data_frame()
    
    success <- function(res) {
      url <-
        res$url
      
      if (return_message) {
        glue::glue("Parsing {url}") %>%
          message()
      }
      .parse_market_trend_url_safe <-
        purrr::possibly(.parse_market_trend_url , data_frame())
      
      all_data <-
        .parse_market_trend_url_safe(url = url)
      
      
      df <<-
        df %>%
        bind_rows(all_data)
    }
    failure <- function(msg) {
      data_frame()
    }
    urls %>%
      map(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }

#' Acquires market trend data
#' 
#' This function returns market
#' trend information dating back to 2015
#' for the user's specified locations
#'
#' @param location_names vector of location names, location name must contain
#' a city name and a comma ie "Brooklyn, NY" or a zipcode
#' @param return_wide if \code{TRUE} returns data in wide form
#' @param return_message if \code{TRUE} returns a message
#' @param ... extra parameters
#'
#' @return a \code{data_frame}
#' @export
#'
#' @examples
#' get_markets_trends(location_names = c("Greenwich, CT", "New London, CT", "Woodside, CA"), return_message = F, return_wide = T)
get_markets_trends <- 
  function(location_names = NULL,
           return_wide = TRUE,
           return_message = F,
           ...) {
    
    if (location_names %>% purrr::is_null()) {
      stop("Please enter locations")
    }
    
    if (location_names %>% purrr::is_null()) {
      stop("Please enter location names!!")
    }
    
    .generate_market_trend_urls_safe <-
      purrr::possibly(.generate_market_trend_urls, data_frame())
    
    df_urls <-
      .generate_market_trend_urls_safe(location_names = location_names)
    
    if (df_urls %>% nrow() == 0) {
      "No results" %>% message()
      return(invisible())
    }
    
    .parse_market_trend_urls_safe <-
      purrr::possibly(.parse_market_trend_urls, data_frame())
    
    all_data <-
      .parse_market_trend_urls_safe(urls = df_urls$urlAPI, return_message = return_message)
      
    all_data <- 
      all_data %>% 
      left_join(df_urls) %>% 
      dplyr::select(one_of("dateData","locationSearch", "stateSearch", "citySearch", "zipcodeSearch"), everything()) %>% 
      arrange(dateData) %>% 
      suppressMessages()
    
    if (all_data %>% nrow() == 0) {
      "No results" %>% message()
      return(invisible())
    }
    
    if (!return_wide) {
      gather_cols <- 
        c("stateSearch", "citySearch",
          "zipcodeSearch", "dateData", "yearData",
          "monthData", "temperatureMarket", "typeSalesToInventoryFavor",
          "urlAPI")
      
      gather_cols <- gather_cols[gather_cols %in% names(all_data)]
      all_data <- 
        all_data %>% 
        gather(metric, value, -gather_cols,na.rm = T)
    }
    
    all_data
    
  }

# validate -----------------------------------------------------------------

.parse_validation_url <-
  function(url = "https://www.realtor.com/validate_geo?location=Easton%2C+MD&retain_secondary_facets=true&include_zip=false&search_controller=Search%3A%3APropertiesController") {
    data <-
      url %>%
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
  function(urls ="https://www.realtor.com/validate_geo?location=Easton%2C+MD&retain_secondary_facets=true&include_zip=false&search_controller=Search%3A%3APropertiesController",
           return_message = TRUE) {
    df <-
      data_frame()
    
    success <- function(res) {
      url <-
        res$url
      
      if (return_message) {
        glue::glue("Parsing {url}") %>%
          message()
      }
      .parse_validation_url_safe <-
        purrr::possibly(.parse_validation_url , data_frame())
      
      all_data <-
        .parse_validation_url_safe(url = url)
      
      
      df <<-
        df %>%
        bind_rows(all_data)
    }
    failure <- function(msg) {
      data_frame()
    }
    urls %>%
      map(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }


.generate_market_validation_url <-
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
        glue::glue(
          "https://www.realtor.com/validate_geo?location={location_slug}&retain_secondary_facets=true&include_zip=false&search_controller=Search%3A%3APropertiesController"
        ) %>%
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
        glue::glue(
          "https://www.realtor.com/validate_geo?location={location_name}&retain_secondary_facets=true&include_zip=false&search_controller=Search%3A%3APropertiesController"
        ) %>%
        as.character()
      
      data <-
        data %>%
        mutate(urlAPI = url)
    }
    data %>% 
      select(
        one_of(
        c(
          "locationSearch",
          "nameCitySeach",
          "stateSearch",
          "zipcodeSearch",
          "urlAPI"
        ),
        everything()
      )) %>% 
      suppressWarnings()
    
  }

.generate_market_validation_urls <-
  function(location_names = c("Bethesda, MD", 20852)) {
    location_names %>% 
      map_df(function(location_name){
        .generate_market_validation_url(location_name = location_name)
      })
  }

validate_realtor_locations <- 
  function(location_names = NULL,
           return_message = TRUE,
           ...) {
    if (location_names %>% purrr::is_null()) {
      stop("Please enter location names!!")
    }
    
    .generate_market_validation_urls_safe <-
      purrr::possibly(.generate_market_validation_urls, data_frame())
    
    df_urls <-
      .generate_market_validation_urls_safe(location_names = location_names)
    
    if (df_urls %>% nrow() == 0) {
      "No results" %>% message()
      return(invisible())
    }
    
    .parse_validation_urls_safe <-
      purrr::possibly(.parse_validation_urls, data_frame())
    
    all_data <-
      .parse_validation_urls(urls = df_urls$urlAPI, return_message = return_message) %>% 
      mutate()
    
    if (all_data %>% nrow() == 0) {
      "No results" %>% message()
      return(invisible())
    }
    
    all_data <-
      all_data %>%
      left_join(df_urls) %>%
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
        areaSF = areaSF %>% readr::parse_number(),
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
    df <-
      data_frame()
    
    success <- function(res) {
      url <-
        res$url
      
      if (return_message) {
        glue::glue("Parsing {url}") %>%
          message()
      }
      .parse_market_vitality_url_safe <-
        purrr::possibly(.parse_market_vitality_url , data_frame())
      
      all_data <-
        .parse_market_vitality_url_safe(url = url)
      
      
      df <<-
        df %>%
        bind_rows(all_data)
    }
    failure <- function(msg) {
      data_frame()
    }
    urls %>%
      map(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
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
  function(location_names = c("Bethesda, MD")) {
    .generate_market_vitality_url_safe <-
      purrr::possibly(.generate_market_vitality_url, data_frame())
    
    location_names %>%
      map_df(function(location_name) {
        .generate_market_vitality_url_safe(location_name = location_name)
      })
  }


#' Meta market vitality data
#'
#' Acquires meta level information
#' for specified locations as of the
#' most recent time period
#'
#' @param location_names vector of location names, location name must contain
#' a city name and a comma ie "Brooklyn, NY" or a zipcode
#'
#' @param return_message if \code{TRUE} returns a message
#' @param ...
#'
#' @return a \code{data_frame}
#' @export
#'
#' @examples
#' get_markets_vitality(location_names = c("La Jolla, CA", "Manhattan, NY", "Bethany, DE", "10016"))
get_markets_vitality <-
  function(location_names = NULL,
           return_message = TRUE,
           ...) {
    if (location_names %>% purrr::is_null()) {
      stop("Please enter location names!!")
    }
    
    .generate_market_vitality_urls_safef <-
      purrr::possibly(.generate_market_vitality_urls, data_frame())
    
    df_urls <-
      .generate_market_vitality_urls_safef(location_names = location_names)
    
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
      left_join(df_urls) %>%
      select(one_of(
        c(
          "locationSearch",
          "nameCity",
          "stateSearch",
          "zipcodeSearch"
        )
      ), everything()) %>%
      suppressMessages() %>%
      suppressWarnings() %>%
      remove_na()
    
    
    all_data
  }

# api ---------------------------------------------------------------------
## https://www.realtor.com/browse_modules/homes_near_street?price=239000&postal_code=20816&median_price=1132000&type=homes_near_street&vis_id=6b0d44ae-f4d6-41f0-8feb-e6491ab43fe9&mcm_id=03714656198478469204855792545062287725&address=5301+Westbard+Cir+Apt+323&user_id=&city=Bethesda&coordinates=38.964595%2C-77.109017


.generate_address_url <- 
  function() {
    base_url <- 'https://www.realtor.com/browse_modules/homes_near_street?'
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
           radius = NULL
           ) {
    
   page <- url %>% read_html()
   
   pages <- page %>% .calculate_pages()
    
    1:pages %>% 
      map_df(function(x){
        
        if (!radius %>% purrr::is_null()) {
          radius_slug <- 
            glue::glue('/radius-{radius}') %>% as.character()
          hasRadius <- T
        } else {
          radius_slug <- ''
          hasRadius <- F
        }
        
        data_frame(idPage = x, 
                   hasRadius,
                   urlListingPage = glue::glue("{url}/pg-{x}{radius_slug}") %>% as.character())
      })
  }

.parse_search_page <- 
  function(url = "https://www.realtor.com/realestateandhomes-search/Bethesda_MD/pg-3") {
    page <- 
      url %>% 
      read_html()
    
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
              actual_name = "nameBroker"
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
              actual_name = "areaSF",
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
          .munge_realtor()
        
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
  function(urls = "https://www.realtor.com/realestateandhomes-search/Bethesda_MD/pg-10", return_message = TRUE) {
    df <-
      data_frame()
    
    success <- function(res) {
      url <-
        res$url
      
      if (return_message) {
        glue::glue("Parsing {url}") %>%
          message()
      }
      .parse_search_page_safe <-
        purrr::possibly(.parse_search_page , data_frame())
      
      all_data <-
        .parse_search_page_safe(url = url)
      
      
      df <<-
        df %>%
        bind_rows(all_data)
    }
    failure <- function(msg) {
      data_frame()
    }
    urls %>%
      map(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }

get_location_data <- 
  function(location_name = c("Bethesda, MD"),
           include_features = F,
           radius = NULL,
           include_property_details = T,
           return_message = T
  ) {
    df_val <- validate_realtor_locations(location_names = location_name)
    
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
    
    if (include_property_details) {
      "Parsing indvidual property listings" %>% message()
      
      .parse_listing_urls_safe <-
        purrr::possibly(.parse_listing_urls, data_frame())
      
      urls <- 
        all_data$urlListing
      
      all_listing <-
        .parse_listing_urls_safe(
          urls = urls,
          include_features = include_features,
          return_message = return_message
        )
      
      all_data <- 
        all_data %>% 
        left_join(all_listing) %>% 
        suppressMessages()
    }
    all_data
  }

# listings ----------------------------------------------------------------

.parse_listing_url <-
  function(url = "https://www.realtor.com/realestateandhomes-detail/5301-Westbard-Cir-Apt-323_Bethesda_MD_20816_M63437-59115",
           include_features = T) {
    page <-
      url %>%
      httr::GET() %>% 
      read_html()
    
    fact_nodes <- 
      page %>% 
      html_nodes(".ldp-key-fact-item")
    
    data <- 
      seq_along(fact_nodes) %>% 
      map_df(function(x){
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
        
        data_frame(numberItem = x, id, value = value %>% as.character())
        
      })
    
    data <- 
      data %>% 
      left_join(dictionary_css_page()) %>% 
      select(-c(numberItem, id)) %>% 
      spread(nameActual, value) %>% 
      .munge_realtor() %>% 
      suppressMessages()
    
    descriptionText <- page %>% html_nodes("#ldp-detail-romance") %>% html_text()
    
    feature_nodes <- page %>% html_nodes('.title-subsection-sm+ .row li')
    
    if (descriptionText %>% length() > 0) {
      data <- 
        data %>% 
        mutate(descriptionText = descriptionText %>% str_to_title())
    }
    
    if (feature_nodes %>% length() > 0 && include_features) {
      features <- feature_nodes %>% html_text()
      data <- 
        data %>% 
        mutate(listFeatures = list(features))
    }
    
    url_vr <- page %>% html_nodes('#hero-view-tour') %>% html_attr('href')
    
    if (url_vr %>% length() > 0) {
      data <- 
        data %>% 
        mutate(urlVRTour = url_vr)
      
    }
    
    url3d <- page %>% html_nodes('.ldp-matterport-label .margin-right-sm') %>% html_attr('href')
    
    if (url3d %>% length() > 0) {
      data <- 
        data %>% 
        mutate(url3DTour = url3d)
      
    }
    
    
    data %>% 
      mutate(urlListing = url)
    
  }

.parse_listing_urls <-
  function(urls = "https://www.realtor.com/realestateandhomes-detail/6817-Connecticut-Ave_Chevy-Chase_MD_20815_M54141-71551",
           include_features = F,
           return_message = TRUE) {
    df <-
      data_frame()
    
    success <- function(res) {
      url <-
        res$url
      
      if (return_message) {
        glue::glue("Parsing {url}") %>%
          message()
      }
      .parse_listing_url_safe <-
        purrr::possibly(.parse_listing_url , data_frame())
      
      all_data <-
        .parse_listing_url(url = url, include_features = include_features)
      
      
      df <<-
        df %>%
        bind_rows(all_data)
    }
    failure <- function(msg) {
      data_frame()
    }
    urls %>%
      map(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }
