.add_date <- 
  function(data) {
    data %>% 
      mutate(dateData = Sys.Date()) %>% 
      dplyr::select(dateData, everything())
  }

.resolve_names <- 
  function(data) {
    realtor_names <- names(data)
    dict_names <- 
      dictionary_realtor_names()
    realtor_names %>% 
      map_chr(function(name){
        dict_name <- dict_names %>% filter(nameRealtor == name)
        if (nrow(dict_name) == 0) {
          glue("Missing {name}") %>% cat(fill = T)
          return(name)
        }
        dict_name %>% slice(1) %>% 
          pull(nameActual)
      })
  }

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
      c("Mozilla/5.0 (Linux; Android 7.0; SM-G892A Build/NRD90M; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/60.0.3112.107 Mobile Safari/537.36", 
        "Mozilla/5.0 (Linux; Android 7.0; SM-G930VC Build/NRD90M; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/58.0.3029.83 Mobile Safari/537.36", 
        "Mozilla/5.0 (Linux; Android 6.0.1; SM-G935S Build/MMB29K; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/55.0.2883.91 Mobile Safari/537.36", 
        "Mozilla/5.0 (Linux; Android 6.0.1; SM-G920V Build/MMB29K) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.98 Mobile Safari/537.36", 
        "Mozilla/5.0 (Linux; Android 5.1.1; SM-G928X Build/LMY47X) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.83 Mobile Safari/537.36", 
        "Mozilla/5.0 (Linux; Android 6.0.1; Nexus 6P Build/MMB29P) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.83 Mobile Safari/537.36", 
        "Mozilla/5.0 (Linux; Android 7.1.1; G8231 Build/41.2.A.0.219; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/59.0.3071.125 Mobile Safari/537.36", 
        "Mozilla/5.0 (Linux; Android 6.0.1; E6653 Build/32.2.A.0.253) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.98 Mobile Safari/537.36", 
        "Mozilla/5.0 (Linux; Android 6.0; HTC One X10 Build/MRA58K; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/61.0.3163.98 Mobile Safari/537.36", 
        "Mozilla/5.0 (Linux; Android 6.0; HTC One M9 Build/MRA58K) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.98 Mobile Safari/537.36", 
        "Mozilla/5.0 (iPhone; CPU iPhone OS 11_0 like Mac OS X) AppleWebKit/604.1.38 (KHTML, like Gecko) Version/11.0 Mobile/15A372 Safari/604.1", 
        "Mozilla/5.0 (iPhone; CPU iPhone OS 11_0 like Mac OS X) AppleWebKit/604.1.34 (KHTML, like Gecko) Version/11.0 Mobile/15A5341f Safari/604.1", 
        "Mozilla/5.0 (iPhone; CPU iPhone OS 11_0 like Mac OS X) AppleWebKit/604.1.38 (KHTML, like Gecko) Version/11.0 Mobile/15A5370a Safari/604.1", 
        "Mozilla/5.0 (iPhone9,3; U; CPU iPhone OS 10_0_1 like Mac OS X) AppleWebKit/602.1.50 (KHTML, like Gecko) Version/10.0 Mobile/14A403 Safari/602.1", 
        "Mozilla/5.0 (iPhone9,4; U; CPU iPhone OS 10_0_1 like Mac OS X) AppleWebKit/602.1.50 (KHTML, like Gecko) Version/10.0 Mobile/14A403 Safari/602.1", 
        "Mozilla/5.0 (Apple-iPhone7C2/1202.466; U; CPU like Mac OS X; en) AppleWebKit/420+ (KHTML, like Gecko) Version/3.0 Mobile/1A543 Safari/419.3", 
        "Mozilla/5.0 (Windows Phone 10.0; Android 6.0.1; Microsoft; RM-1152) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.116 Mobile Safari/537.36 Edge/15.15254", 
        "Mozilla/5.0 (Windows Phone 10.0; Android 4.2.1; Microsoft; RM-1127_16056) AppleWebKit/537.36(KHTML, like Gecko) Chrome/42.0.2311.135 Mobile Safari/537.36 Edge/12.10536", 
        "Mozilla/5.0 (Windows Phone 10.0; Android 4.2.1; Microsoft; Lumia 950) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2486.0 Mobile Safari/537.36 Edge/13.10586", 
        "Mozilla/5.0 (Linux; Android 7.0; Pixel C Build/NRD90M; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/52.0.2743.98 Safari/537.36", 
        "Mozilla/5.0 (Linux; Android 6.0.1; SGP771 Build/32.2.A.0.253; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/52.0.2743.98 Safari/537.36", 
        "Mozilla/5.0 (Linux; Android 6.0.1; SHIELD Tablet K1 Build/MRA58K; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/55.0.2883.91 Safari/537.36", 
        "Mozilla/5.0 (Linux; Android 7.0; SM-T827R4 Build/NRD90M) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.116 Safari/537.36", 
        "Mozilla/5.0 (Linux; Android 5.0.2; SAMSUNG SM-T550 Build/LRX22G) AppleWebKit/537.36 (KHTML, like Gecko) SamsungBrowser/3.3 Chrome/38.0.2125.102 Safari/537.36", 
        "Mozilla/5.0 (Linux; Android 4.4.3; KFTHWI Build/KTU84M) AppleWebKit/537.36 (KHTML, like Gecko) Silk/47.1.79 like Chrome/47.0.2526.80 Safari/537.36", 
        "Mozilla/5.0 (Linux; Android 5.0.2; LG-V410/V41020c Build/LRX22G) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/34.0.1847.118 Safari/537.36", 
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.135 Safari/537.36 Edge/12.246", 
        "Mozilla/5.0 (X11; CrOS x86_64 8172.45.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.64 Safari/537.36", 
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/601.3.9 (KHTML, like Gecko) Version/9.0.2 Safari/601.3.9", 
        "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36", 
        "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:15.0) Gecko/20100101 Firefox/15.0.1", 
        "Mozilla/5.0 (CrKey armv7l 1.5.16041) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.0 Safari/537.36", 
        "Roku4640X/DVP-7.70 (297.70E04154A)", "Mozilla/5.0 (Linux; U; Android 4.2.2; he-il; NEO-X5-116A Build/JDQ39) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Safari/534.30", 
        "Mozilla/5.0 (Linux; Android 5.1; AFTS Build/LMY47O) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/41.99900.2250.0242 Safari/537.36", 
        "Dalvik/2.1.0 (Linux; U; Android 6.0.1; Nexus Player Build/MMB29T)", 
        "AppleTV6,2/11.1", "AppleTV5,3/9.1.1", "Mozilla/5.0 (Nintendo WiiU) AppleWebKit/536.30 (KHTML, like Gecko) NX/3.0.4.2.12 NintendoBrowser/4.3.1.11264.US", 
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64; XBOX_ONE_ED) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.79 Safari/537.36 Edge/14.14393", 
        "Mozilla/5.0 (Windows Phone 10.0; Android 4.2.1; Xbox; Xbox One) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2486.0 Mobile Safari/537.36 Edge/13.10586", 
        "Mozilla/5.0 (PlayStation 4 3.11) AppleWebKit/537.73 (KHTML, like Gecko)", 
        "Mozilla/5.0 (PlayStation Vita 3.61) AppleWebKit/537.73 (KHTML, like Gecko) Silk/3.2", 
        "Mozilla/5.0 (Nintendo 3DS; U; ; en) Version/1.7412.EU", "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)", 
        "Mozilla/5.0 (compatible; bingbot/2.0; +http://www.bing.com/bingbot.htm)", 
        "Mozilla/5.0 (compatible; Yahoo! Slurp; http://help.yahoo.com/help/us/ysearch/slurp)", 
        "Mozilla/5.0 (X11; U; Linux armv7l like Android; en-us) AppleWebKit/531.2+ (KHTML, like Gecko) Version/5.0 Safari/533.2+ Kindle/3.0+", 
        "Mozilla/5.0 (Linux; U; en-US) AppleWebKit/528.5+ (KHTML, like Gecko, Safari/528.5+) Version/4.0 Kindle/3.0 (screen 600x800; rotate)"
      )
    
    
    
    user_agent <-
      user_agents[!user_agents %>% str_detect("bot|slurp")] %>%
      sample(1)
    
    tl_domain <-
      c('.com', '.gov', '.org') %>%
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
      reduce(paste0)
    df <-
      tibble(urlReferer = url,
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
    
    data <- tibble(addressProperty = address, 
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
      seq_along(page_attributes) %>%
      map_dfr(function(x) {
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
      value <- parse_number(as.character(value))
    }
    tibble(nameActual = actual_name, value)
  }


#' Join meta level listing data and detailed listing data
#'
#' @param data_listings \code{tibble} from \code{listings} function
#' @param data_detailed_listings \code{tibble} from \code{parse_listing_urls}
#'
#' @return a \code{tibble}
#' @export
#'
#' @examples
join_listing_data <- 
  function(data_listings, data_detailed_listings) {
    data <- 
      data_detailed_listings %>%
      select(-one_of(
        c(
          "statusListing",
          "urlImage",
          "addressProperty",
          "cityProperty",
          "stateProperty",
          "zipcodeProperty",
          "priceListing",
          "areaPropertySF",
          "countBaths",
          "countBeds",
          "sizeLotAcres",
          "nameBrokerage",
          "pricePerSFListing"
        )
      )) %>%
      left_join(data_listings) %>% 
      suppressMessages()
    
    data <-
      data %>%
      mutate(idRow = 1:n()) %>%
      left_join(
        data_detailed_listings %>%
          mutate(idRow = 1:n()) %>%
          filter(hasPhotos) %>%
          dplyr::select(idRow, dataPhotos) %>%
          unnest() %>%
          group_by(idRow) %>%
          sample_n(1) %>%
          ungroup()
      ) %>%
      select(-idRow) %>%
      suppressMessages()
    data <-
      data %>% 
      mutate(urlImage = case_when(is.na(urlImage) ~ urlPhotoProperty,
                                  TRUE ~ urlImage))
    data
  }