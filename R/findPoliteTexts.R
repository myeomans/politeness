#' Find polite text
#' @description Finds examples of most or least polite text in a corpus
#' @param text a character vector of texts.
#' @param df_polite a data.frame with politeness features, as outputed by \code{\link{politeness}}, used to train model.
#' @param covar a vector of politeness labels, or other covariate.
#' @param type a string indicating if function should return the most or least polite texts or both. If \code{length > 1} only first value is used.
#' @param num_docs integer of number of documents to be returned. Default is 5.
#' @param ... additional parameters to be passed to \code{politenessProjection}.
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
#' findPoliteTexts(phone_offers$message,
#'                 polite.data,
#'                 phone_offers$condition,
#'                 type = "most",
#'                 num_docs = 5)
#'
#' findPoliteTexts(phone_offers$message,
#'                 polite.data,
#'                 phone_offers$condition,
#'                 type = "least",
#'                 num_docs = 10)
#'
#'@export

findPoliteTexts <- function(text,
                            df_polite,
                            covar,
                            type = c("most","least","both"),
                            num_docs = 5L,
                            ...){


  # check type
  valid_type <- c("most","least","both")
  type <- type[1] # in case type has length > 1 only use first entry
  if( ! type %in% valid_type){
    stop( paste0("type must be one of the following ", paste0(valid_type, collapse = ", ")))
  }

  l_proj <- suppressWarnings(politenessProjection(df_polite_train = df_polite,
                                 covar = covar, ... ))

  m_train_proj <- as.vector(l_proj$train_proj)
  df_docs_proj <- data.frame(text = text, projection = m_train_proj)

  if(type %in% c("most","least")){
    is_most <-  type == "most"
    df_docs_proj <- df_docs_proj[ order(df_docs_proj$projection, decreasing = is_most) , ]
    df_out <- df_docs_proj[ 1:num_docs , "text", drop = FALSE ]
    df_out$rank <-  1:num_docs
  } else {

    num_docs_sub <- num_docs %/% 2
    # check if num_docs is even
    num_most <- ifelse( num_docs %% 2 == 0 , num_docs_sub, num_docs_sub + 1    )
    num_least <- num_docs_sub

    df_docs_proj <- df_docs_proj[ order(df_docs_proj$projection, decreasing = TRUE) , ]
    df_most_polite <- df_docs_proj[ 1:num_most , "text", drop = FALSE ]
    df_most_polite$rank <-  1:num_most
    df_most_polite$group <- "most"

    df_docs_proj <- df_docs_proj[ order(df_docs_proj$projection, decreasing = FALSE) , ]
    df_least_polite <- df_docs_proj[ 1:num_least , "text", drop = FALSE ]
    df_least_polite$rank <-  1:num_least
    df_least_polite$group <- "least"

    df_out <- rbind(df_most_polite, df_least_polite)
  }

  row.names(df_out) <- NULL
  return(df_out)
}
