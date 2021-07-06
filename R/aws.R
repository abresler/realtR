dictionary_geo_names <-
  function() {
    tibble(
      nameGeo = c(
        "area_type",
        "_id",
        "_score",
        "city",
        "state_code",
        "country",
        "centroid.lon",
        "centroid.lat",
        "neighborhood",
        "postal_code",
        "has_catchment",
        "school_id",
        "school",
        "line",
        "full_address",
        "prop_status",
        "street",
        "mpr_id",
        "street_number",
        "street_name",
        "street_suffix",
        "street_dir",
        "street_post_dir",
        "counties",
        "slug_id",
        "geo_id",
        "county_needed_for_uniq",
        "city_slug_id"
      ),
      nameActual =  c(
        "typeArea",
        "slugLocation",
        "scorePrediction",
        "nameCity",
        "slugState",
        "codeCountry",
        "longitudeLocation",
        "latitudeLocation",
        "nameNeighborhood",
        "zipcodeLocation",
        "hasCatchment",
        "idSchool",
        "nameSchool",
        "addressStreet",
        "nameAddress",
        "statusProperty",
        "nameStreet",
        "idMPR",
        "numberStreet",
        "nameStreetAbbr",
        "suffixStreet",
        "directionStreet",
        "directionStreetPost",
        
        "counties",
        "hasSlug",
        "hashGeo",
        "hasCountyNeededForUniq",
        "slugCity"
      )
    )
  }

#  gdeltr2::load_needed_packages(required_packages = c("dplyr", "glue", "stringr", "jsonlite", "curl", "rvest", "purrr", "requestsR"))
.parse_geo_query <-
  function(url = "https://parser-external.geo.moveaws.com/suggest?input=Gre&limit=100&client_id=rdcV8&area_types=neighborhood%2Ccity%2Ccounty%2Cpostal_code%2Caddress%2Cbuilding%2Cstreet%2Cschool%2CFuck") {
    data <-
      url %>%
      .curl_json() %>%
      fromJSON(
        simplifyVector = T,
        simplifyDataFrame = T,
        flatten = T
      )
    
    data <-
      data$autocomplete %>%
      as_tibble()
    df_names <- dictionary_geo_names()
    actual_names <-
      names(data) %>%
      map_chr(function(name) {
        df_row <- df_names %>% filter(nameGeo == name)
        if (df_row %>% nrow() == 0) {
          glue("Missing {name}") %>%
            cat(fill = T)
          return(name)
        }
        df_row %>% pull(nameActual)
      })
    
    data <-
      data %>%
      set_names(actual_names) %>%
      mutate(urlGeoAPI = url)
    
    if (data %>% has_name("nameAddress")) {
      data <-
        data %>%
        mutate(nameAddress = nameAddress %>% map_chr(function(x) {
          if (x %>% is_null()) {
            return(NA)
          }
          x[[1]] %>% str_c(collapse = ", ")
        }))
    }
    
    if (data %>% has_name("statusProperty")) {
      data <-
        data %>%
        mutate(statusProperty = statusProperty %>% map_chr(function(x) {
          if (x %>% is_null()) {
            return(NA)
          }
          x[[1]] %>% str_c(collapse = ", ")
        }))
    }
    
    
    data <-
      data %>%
      mutate_at(data %>% dplyr::select(dplyr::matches("^id[A-Z]")) %>% names(),
                funs(. %>% as.numeric()))
    
    data
  }

.generate_geo_url <-
  function(area_name = "Beth",
           limit = 100,
           search_types = c(
             "neighborhood",
             "city",
             "county",
             "postal_code",
             "address",
             "building",
             "street",
             "school"
           )) {
    area_name <- as.character(area_name)
    if (limit > 100) {
      stop("limit cannot exceed 100")
    }
    client_id <- 'rdcV8'
    base <- 'https://parser-external.geo.moveaws.com/suggest?input='
    area_types <-
      search_types %>% str_c(collapse = ",")
    search_area <-
      URLencode(area_name)
    
    url <-
      glue(
        "{base}{search_area}&limit={limit}&client_id={client_id}&area_types={area_types}"
      ) %>%
      as.character() %>%
      URLencode()
    tibble(nameLocationSearch = area_name,
               urlGeoAPI = url)
  }

generate_geo_urls <-
  function(locations = c("Greenwich", "Bethesda"),
           search_types = c(
             "neighborhood",
             "city",
             "county",
             "postal_code",
             "address",
             "building",
             "street",
             "school"
           ),
           limit = 100) {
    generate_geo_url_safe <-
      possibly(.generate_geo_url, tibble())
    
    locations %>%
      map_dfr(function(area) {
        .generate_geo_url(area_name = area,
                          limit = limit,
                          search_types = search_types)
      })
  }

#' Parse GEO URLS
#'
#' @param urls 
#' @param use_future 
#' @param return_message 
#'
#' @return
#' @export
#'
#' @examples
parse_geo_urls <-
  function(urls = "https://parser-external.geo.moveaws.com/suggest?input=bethesda&limit=100&client_id=rdcV8&area_types=neighborhood,city,county,postal_code,address",
           use_future = F,
           return_message = T) {
    .parse_geo_query_safe <-
      possibly(.parse_geo_query, tibble())
    
    if (!use_future) {
    all_data <- 
      urls %>%
      map_dfr(function(url) {
        if (return_message) {
          glue("Parsing {url %>% str_replace_all('https://www.realtor.com/', '')}") %>%
            message()
        }
        .parse_geo_query_safe(url = url)
      })
    }
    
    if (use_future) {
      options(future.globals.maxSize = 999999 * 1024 ^ 12)
      future::plan(cluster)
      all_data <- 
        urls %>%
        furrr::future_map_dfr(function(url) {
          .parse_geo_query_safe(url = url)
        })
      closeAllConnections()
    }
    
    
    
    all_data
  }

#' Location geocoder
#'
#' This function geocodes a users vector of locations
#' and returns a \code{tibble} with the corresponding results
#'
#' @param locations vector of locations
#' @param search_types  vector of search parameters options include \itemize{
#' \item neighborhood - includes neighborhood information
#' \item city - includes city information
#' \item county - includes county information
#' \item postal_code - includes zipcode
#' \item building - include building info
#' \item street - include street info
#' \item school - include school info
#' }
#' @param limit numeric vector of results cannot exceed 100
#' @param return_message if \code{TRUE} returns a message
#' @param use_future 
#' @param ... extra parameters
#' @param snake_names 
#' @param remove_list_columns 
#'
#' @family geocoder
#' @return a \code{tibble}
#' @export
#'
#' @examples
#' geocode(locations = c("Palm Springs", "Bethesda", 10016), limit = 100)
geocode <-
  function(locations = NULL,
           search_types = c(
             "neighborhood",
             "city",
             "county",
             "postal_code",
             "address",
             "building",
             "street",
             "school"
           ),
           use_future = F,
           snake_names = F,
           limit = 100,
           remove_list_columns = F,
           return_message = TRUE,
           ...) {
    if (length(locations) == 0) {
      stop("Please enter search areas")
    }
    df_urls <-
      generate_geo_urls(locations = locations,
                        search_types = search_types,
                        limit = 100)
    
    all_data <-
      parse_geo_urls(urls = df_urls$urlGeoAPI, return_message = return_message,
                     use_future = use_future)
    
    all_data <-
      all_data %>%
      left_join(df_urls, by = "urlGeoAPI") %>%
      select(nameLocationSearch, everything())
    
    all_data <-
      all_data %>%
      mutate_if(is.character, function(x) {
        case_when(x == "" ~ NA_character_,
                  TRUE ~ x)
      }) %>%
      janitor::remove_empty(which = "cols")
    
    if (remove_list_columns) {
      list_cols <- data %>% select_if(is.list) %>% names()
      if (length(list_cols) > 0) {
        data <- data %>% 
          select(-one_of(list_cols))
      }
    }
    
    if (snake_names) {
      all_data <- 
        janitor::clean_names(all_data)
    }
    
    all_data
  }