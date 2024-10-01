#' Politeness projection
#'
#' @description Deprecated. Function is now called \code{\link{trainModel}}.
#' @param df_polite_train a data.frame with politeness features as outputed by \code{\link{politeness}} used to train model.
#' @param covar a vector of politeness labels, or other covariate.
#' @param ... additional parameters to be passed. See \code{\link{trainModel}}.
#' @return list of model objects.
#' @details See \code{\link{trainModel}} for details.
#' @md
#' @examples
#'
#' data("phone_offers")
#' data("bowl_offers")
#'
#' polite.data<-politeness(phone_offers$message, parser="none",drop_blank=FALSE)
#'
#' polite.holdout<-politeness(bowl_offers$message, parser="none",drop_blank=FALSE)
#'
#' project<-politenessProjection(polite.data,
#'                               phone_offers$condition,
#'                               polite.holdout)
#'
#' # Difference in average politeness across conditions in the new sample.
#'
#' mean(project$test_proj[bowl_offers$condition==1])
#' mean(project$test_proj[bowl_offers$condition==0])
#'
#' @import glmnet
#' @export


politenessProjection <- function(df_polite_train, covar = NULL,
                                 ...){

  return(trainModel(df_polite_train, covar, ...))

}
