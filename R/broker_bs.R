.broker_bs <- function(text,
                      tagger,
                      sentences = 1,
                      return_message = TRUE) {
  
  df_udpipe <-
    udpipe_annotate(object = tagger, x = text) %>% as.data.frame() %>% as_tibble()
  
  original_sentences <- df_udpipe$sentence_id %>% max()
    
  if (return_message) {
    glue("\n\nCutting down these {original_sentences} sentence(s) of broker bullshit into {sentences} concise sentence(s) using the PageRank algorithm\n") %>% cat(fill = T)
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
    glue("\n\nThis is much better\n\n\n{summary_sents}\n\n-----------\n\n") %>% cat(fill = T)
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
#' @import udpipe textrank
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' library(udpipe)
#' library(textrank)
#' # Download model first: udpipe_download_model(language = "english")
#' tagger <- udpipe_load_model(file = "path/to/english-model.udpipe")
#' broker_bs <- "Beautiful home with stunning views and modern amenities."
#' summarise_broker_bullshit(descriptions = broker_bs, tagger = tagger, sentences = 1)
#' }
#' 
summarise_broker_bullshit <-
  function(descriptions,
           tagger,
           sentences = 1,
           return_message = TRUE) {
    descriptions <-
      descriptions[!descriptions %>% is.na()]
    .broker_bs_safe <- 
      possibly(.broker_bs, tibble())
    descriptions %>%
      map_dfr(function(text) {
        resp <- .broker_bs(
          text = text,
          tagger = tagger,
          sentences = sentences,
          return_message = return_message
        )
        tibble(descriptionText = text, textTextRank = resp)
      })
  }