#' Find polite text
#' @description Finds examples of most or least polite text in a corpus
#' @param text a character vector of texts.
#' @param covar a vector of politeness labels (from human or model), or other covariate.
#' @param type a string indicating if function should return the most or least polite texts or both. If \code{length > 1} only first value is used.
#' @param num_docs integer of number of documents to be returned. Default is 5.
#' @return data.frame with texts ranked by (more or least) politeness. See details for more information.
#' @details Function returns a data.frame ranked by (more or least) politeness.
#' If \code{type == 'most'}, the \code{num_docs} most polite texts will be returned.
#' If \code{type == 'least'}, the \code{num_docs} least polite texts will be returned.
#' If \code{type == 'both'}, both most and least polite text will be returned.
#' if \code{num_docs} is even, half will be most and half least polite else half + 1 will be most polite.
#'
#' \code{df_polite} must have the same number of rows as the \code{length(text)} and \code{length(covar)}.
#' @examples
#'
#' data("phone_offers")
#' polite.data<-politeness(phone_offers$message, parser="none",drop_blank=FALSE)
#'
#' exampleTexts(phone_offers$message,
#'                 phone_offers$condition,
#'                 type = "most",
#'                 num_docs = 5)
#'
#' exampleTexts(phone_offers$message,
#'                 phone_offers$condition,
#'                 type = "least",
#'                 num_docs = 10)
#'
#'@export

exampleTexts <- function(text,
                         covar,
                         type = c("most","least"),
                         num_docs = 5L){


  # check type
  valid_type <- c("most","least")
  type <- type[1] # in case type has length > 1 only use first entry
  if( ! type %in% valid_type){
    stop( paste0("type must be one of the following ", paste0(valid_type, collapse = ", ")))
  }
  #
  #   l_proj <- suppressWarnings(trainModel(df_polite_train = df_polite,
  #                                                   covar = covar, ... ))
  #
  #   m_train_proj <- as.vector(l_proj$train_proj)
  #   df_docs_proj <- data.frame(text = text, projection = m_train_proj)
  #   dimension <- df_docs_proj$projection
  if(! length(covar) ==length(text)){
    stop("Covariate must be same length as texts")
  }
  dimension=covar

  is_most <-  type == "most"
  df_out <- text[order(dimension, decreasing = is_most)][1:num_docs]
  return(df_out)
}
