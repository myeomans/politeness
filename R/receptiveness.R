utils::globalVariables(c("receptTrain")) # prevent incorrect "no global binding" note

#' Conversational Receptiveness
#'
#' @description Pre-trained model to detect conversational receptiveness
#' @param texts character A vector of texts, each of which will be tallied for politeness features.
#' @param num_mc_cores integer Number of cores for parallelization.
#' @details This is a wrapper around a pre-trained model of "conversational receptiveness".
#' The model trained from Study 1 of that paper can be applied to new text with a single function.
#' This model requires grammar parsing via SpaCy. Please see \code{\link{spacyr}} for details on installation.
#' @return a vector with receptiveness scores
#' @references
#' Yeomans, M., Minson, J., Collins, H., Chen, F. & Gino, F. (2020). Conversational Receptiveness: Improving Engagement with Opposing Views. OBHDP.
#'
#' @examples
#'
#'
#'\dontrun{
#' data("phone_offers")
#'
#' receptiveness(phone_offers$message)
#'
#'}
#'
#'@export
receptiveness<-function(texts, num_mc_cores=1){

  m_polite_test = as.matrix(politeness::politeness(texts,drop_blank = F,
                                                   metric="count",
                                                   parser="spacy",
                                                   num_mc_cores=num_mc_cores))

  polite_predict<-as.vector(stats::predict(politeness::receptive_model,
                                 newx=m_polite_test,
                                 s="lambda.min", type="response"))

  return(polite_predict)
}
