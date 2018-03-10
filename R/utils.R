remove_columns <- 
  function(data, columns = c("urlGeoAPI", "urlAPI")) {
    data %>% 
      dplyr::select(-one_of(columns)) %>% 
      suppressWarnings() %>% 
      suppressMessages()
  }

generate_url_reference <-
  function() {
    user_agents <-
      c(
        "Mozilla/5.0 (Linux; U; en-US) AppleWebKit/528.5+ (KHTML, like Gecko, Safari/528.5+) Version/4.0 Kindle/3.0 (screen 600x800; rotate)",
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.135 Safari/537.36 Edge/12.246",
        "Mozilla/5.0 (X11; CrOS x86_64 8172.45.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.64 Safari/537.36",
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/601.3.9 (KHTML, like Gecko) Version/9.0.2 Safari/601.3.9",
        "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36"
      )
    
    
    user_agent <-
      user_agents[!user_agents %>% str_detect("bot|slurp")] %>%
      sample(1)
    
    tl_domain <-
      c('.com', '.gov', '.org', '.mil', '.co') %>%
      sample(1)
    
    word_length <-
      8:15
    
    words <-
      word_length %>% sample(1)
    
    domain_slug <-
      1:words %>%
      map_chr(function(x) {
        sample(letters, 1)
      }) %>%
      paste0(collapse = '')
    
    url <-
      list('http://', domain_slug, tl_domain) %>%
      purrr::reduce(paste0)
    df <-
      data_frame(urlReferer = url,
                 userAgent = user_agent)
    df
  }

parse_address <- 
  function(address = "4915 Greenway Dr, Bethesda, MD 20816") {
    if (address %>% str_count('\\,') == 2) {
    parts <- 
      address %>% 
      str_split('\\,') %>% 
      flatten_chr() %>% 
      str_trim()
    
    zip_parts <- parts[[3]] %>% str_split('\\ ') %>% flatten_chr()
    
    data <- data_frame(addressProperty = address, 
               stretProperty = parts[[1]],
               cityProperty = parts[[2]],
               slugStateProperty = zip_parts[[1]],
               zipcodeProperty = zip_parts[[2]])
    
    return(data)
    }
    
  }

remove_na <- function(data) {
  data %>%
    dplyr::select(which(colMeans(is.na(.)) < 1))
  
}


discard_text <- function(x) {
  x %>%  str_split("\t|\n") %>%
    flatten_chr() %>%
    discard( ~ .x == "") %>%
    str_to_upper() %>% str_c(collapse =  " ")
}
parse_for_text <- function(page, css = ".soln") {
  page %>%
    html_nodes(css = css) %>%
    html_text() %>%
    str_trim()
}


get_page_attribute_data <-
  function(page) {
    page_attributes <-
      page %>% html_nodes("div") %>%
      html_attrs()
    df_attrs <-
      1:length(page_attributes) %>%
      map_df(function(x) {
        page_attributes[x] %>% flatten_df()
      })
    df_attrs
  }




parse_css_name <-
  function(page,
           css = "#dnf_class_values_procurement_notice__solicitation_number__widget",
           actual_name = "idSolicitation",
           is_numeric = F) {
    if (page %>% html_nodes(css) %>% length() == 0) {
      return(invisible())
    }
    value <-
      page %>%
      parse_for_text(css = css)
    
    if (is_numeric) {
      value <- readr::parse_number(value)
    }
    data_frame(nameActual = actual_name, value)
  }