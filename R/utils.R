
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