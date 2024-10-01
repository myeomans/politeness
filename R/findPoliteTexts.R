#' Find polite text
#'
#' @description Deprecated... This function has a new name now. See exampleTexts for details.
#' @param text a character vector of texts.
#' @param covar a vector of politeness labels, or other covariate.
#' @param ... other arguments passed on to exampleTexts. See exampleTexts for details.
#' @return a ggplot of the prevalence of politeness features, conditional on \code{split}. Features are sorted by variance-weighted log odds ratio.
#' @examples


#' data("phone_offers")
#' polite.data<-politeness(phone_offers$message, parser="none",drop_blank=FALSE)
#'
#' findPoliteTexts(phone_offers$message,
#'                 phone_offers$condition,
#'                 type = "most",
#'                 num_docs = 5)
#'
#'
#'@export

#' @export

findPoliteTexts<-function(text,covar,...){
  exampleTexts(text,covar,...)
}
