dictionary_geo_names <- 
  function() {
    data_frame(nameGeo = c("area_type", "_id", "_score", "city", "state_code", "country", 
                           "centroid.lon", "centroid.lat", 
                           "neighborhood", "postal_code", "has_catchment",
                           "school_id",
                           "school",
                           "line",
                           "full_address",
                           "prop_status",
                           "street",
                           "mpr_id"),
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
                 "idMPR"
               ))
  }

#  gdeltr2::load_needed_packages(required_packages = c("dplyr", "glue", "stringr", "jsonlite", "curl", "rvest", "purrr", "requestsR"))
.parse_geo_query <-
  function(url = "https://parser-external.geo.moveaws.com/suggest?input=Gre&limit=100&client_id=rdcV8&area_types=neighborhood%2Ccity%2Ccounty%2Cpostal_code%2Caddress%2Cbuilding%2Cstreet%2Cschool%2CFuck") {
    data <-
      url %>%
      jsonlite::fromJSON(
        simplifyVector = T,
        simplifyDataFrame = T,
        flatten = T
      )
    
    data <- 
      data$autocomplete %>%
      as_data_frame()
    df_names <- dictionary_geo_names()
    actual_names <- 
      names(data) %>% 
      map_chr(function(name){
        df_row <- df_names %>% filter(nameGeo == name)
        if (df_row %>% nrow() == 0) {
          glue::glue("Missing {name}") %>% 
            message()
          return(name)
        }
        df_row %>% pull(nameActual)
      })
    
    data <- 
      data %>%
      purrr::set_names(
       actual_names
      ) %>% 
      mutate(urlGeoAPI = url)
    
    if (data %>% tibble::has_name("nameAddress")) {
     data <- 
       data %>%
        mutate(nameAddress = nameAddress %>% map_chr(function(x) {
          if (x %>% purrr::is_null()) {
            return(NA)
          }
          x[[1]] %>% str_c(collapse = ", ")
        }))
    }
    
    if (data %>% tibble::has_name("statusProperty")) {
      data <- 
        data %>%
        mutate(statusProperty = statusProperty %>% map_chr(function(x) {
          if (x %>% purrr::is_null()) {
            return(NA)
          }
          x[[1]] %>% str_c(collapse = ", ")
        }))
    }
    
    
    data <- 
      data %>% 
      mutate_at(data %>% dplyr::select(matches("^id[A-Z]")) %>% names(),
                funs(. %>% as.numeric()))
    
    data
  }

.generate_geo_url <- 
  function(area_name = "Beth", 
           limit = 100,
           search_types = c("neighborhood","city","county","postal_code","address","building","street","school")
           ) {
    if (limit > 100) {
      stop("limit cannot exceed 100")
    }
    client_id <- 'rdcV8'
    base <- 'https://parser-external.geo.moveaws.com/suggest?input='
    area_types <- 
      search_types %>% str_c(collapse = ",")
    search_area <- URLencode(area_name)
    
    url <- 
      glue::glue("{base}{search_area}&limit={limit}&client_id={client_id}&area_types={area_types}") %>% 
      as.character() %>% 
      URLencode()
    data_frame(nameLocationSearch = area_name,
               urlGeoAPI = url)
  }

generate_geo_urls <-
  function(area_names = c("Greenwich", "Bethesda"),
           search_types = c("neighborhood","city","county","postal_code","address","building","street","school"),
           limit = 100) {
    generate_geo_url_safe <- 
      purrr::possibly(.generate_geo_url, data_frame())
    
    area_names %>% 
      map_df(function(area){
        .generate_geo_url(area_name = area, limit = limit, search_types = search_types)
      })
  }

parse_geo_urls <-
  function(urls = "https://parser-external.geo.moveaws.com/suggest?input=bethesda&limit=100&client_id=rdcV8&area_types=neighborhood,city,county,postal_code,address",
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
      .parse_geo_query_safe <-
        purrr::possibly(.parse_geo_query , data_frame())
      
      all_data <-
        .parse_geo_query_safe(url = url)
      
      
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

#' Get AWS area geo-query
#' 
#' This function returns data from the Amazon geoquery API
#' for the specified areas
#'
#' @param area_names vector of area names
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
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' get_aws_geo_areas(area_names = c("Palm Springs", "Bethesda"), limit = 100)
get_aws_geo_areas <-
  function(area_names = NULL,
           search_types = c("neighborhood","city","county","postal_code","address","building","street","school"),
           limit = 100,
           return_message = TRUE, 
           ...) {
    
    if (area_names %>% purrr::is_null()) {
      stop("Please enter search areas")
    }
    df_urls <-
      generate_geo_urls(area_names = area_names,
                        search_types = search_types,
                        limit = 100)
    
    all_data <- 
     parse_geo_urls(urls = df_urls$urlGeoAPI, return_message = return_message)
   
    all_data <-
      all_data %>%
      left_join(df_urls) %>%
      select(nameLocationSearch, everything()) %>%
      suppressMessages()
    
    all_data
  }