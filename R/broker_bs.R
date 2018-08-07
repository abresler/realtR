.broker_bs <- function(text,
                      tagger,
                      sentences = 1,
                      return_message = TRUE) {
  
  df_udpipe <-
    udpipe_annotate(object = tagger, x = text) %>% as.data.frame() %>% as_data_frame()
  
  original_sentences <- df_udpipe$sentence_id %>% max()
    
  if (return_message) {
    glue::glue("\n\nCutting down these {original_sentences} sentence(s) of broker bullshit into {sentences} concise sentence(s) using the PageRank algorithm\n") %>% message()
  }
  
  
  keys <-
    df_udpipe$lemma %>%
    textrank_keywords(relevant = df_udpipe$upos %in% c("NOUN", "VERB", "ADJ"))
  
  sents <-
    df_udpipe[, c("sentence_id", "sentence")] %>%
    unique()
  
  terms <-
    df_udpipe %>%
    filter(upos %in% c("NOUN", "ADJ"))
  
  tr <-
    textrank_sentences(data = sents, terminology = terms)
  
  summary_sents <-
    summary(tr, n = sentences) %>% str_c(collapse = "\n")
  
  if (return_message) {
    glue::glue("\n\nThis is much better\n\n\n{summary_sents}\n\n-----------\n\n") %>% message()
  }
  summary_sents
}

#' Clean Broker Bull Shit
#' 
#' This function takes a 
#' vector descriptions and cleans
#' them into a concise sentence or 
#' sentences 
#'
#' @param descriptions vector of descriptions
#' @param tagger a udpipe object - this is the english dictionary on your computer from the udpipe package
#' @param sentences number of sentences to whittle down to
#' @param return_message if \code{TRUE} returns messages
#' @import emo udpipe textrank
#' @return
#' @export
#'
#' @examples
#' 
#' broker_bs <- c("This Tashmoo Woods End-Unit Condo Is Simply A Step Above The Rest. The Light And Bright Living Room Has Soaring Cathedral Ceilings, Fireplace, Hardwood Floors And French Doors That Open To A Deck. Eat-In Kitchen W/Newer Appliances And An Abundance Of Cabinets. Dining Area Off Of Living Room. A Den And Full Bath Finish The 1St Floor. The Second Level Features 2 Spacious Bedrooms With En Suites. The Master Has A Tiled Bath With Jacuzzi Tub, Double Vanity And Shower. One Of The Unique Features Of This Unit Is The Full, Partially Finished, Walkout Basement. Just Out The Front Door Is An Outdoor Shower, Storage Area And Detached Garage. Enormous Deck. Central Ac. Dehumidification System. Conveniently Located To All Of The Association Amenities Which Include A Swimming Pool W/Pool House, Wifi & Lounge Chairs, Tennis Courts, Playground, Basketball Court, Private Beach On The Vineyard Sound And A Small Dock On Lake Tashmoo.easy Access To Town, Ferries, Golf & More.")
#' library(textrank)
#' library(udpipe)
#' tagger <- udpipe_load_model(file = path_to_model) 
#' summarise_broker_bullshit(descriptions = broker_bs, tagger = tagger, sentences = 3, return_message = T)
#' 
#' 

#' 
summarise_broker_bullshit <-
  function(descriptions,
           tagger,
           sentences = 1,
           return_message = TRUE) {
    descriptions <-
      descriptions[!descriptions %>% is.na()]
    .broker_bs_safe <- 
      purrr::possibly(.broker_bs, data_frame())
    descriptions %>%
      map_df(function(text) {
        resp <- .broker_bs(
          text = text,
          tagger = tagger,
          sentences = sentences,
          return_message = return_message
        )
        data_frame(descriptionText = text, textTextRank = resp)
      })
  }