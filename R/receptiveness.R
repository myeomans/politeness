utils::globalVariables(c("receptive_train")) # prevent incorrect "no global binding" note

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
  m_polite_train = as.matrix(politeness::politeness(politeness::receptive_train$text,
                                                    parser="spacy",
                                                    num_mc_cores=num_mc_cores))

  m_polite_test = as.matrix(politeness::politeness(texts,
                                                   parser="spacy",
                                                   num_mc_cores=num_mc_cores))

  polite_predict<-as.vector(politeness::politenessProjection(m_polite_train,
                                                             politeness::receptive_train$receptive,
                                                             m_polite_test)$test_proj)


  return(polite_predict)
}
