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
  train_dat=politeness::receptive_train

  m_polite_train = as.matrix(politeness::receptive_polite)

  m_polite_test = as.matrix(politeness::politeness(texts,
                                                   parser="spacy",
                                                   num_mc_cores=num_mc_cores))

  # Remove 1st/2nd person pronouns
  m_train=m_polite_train[,-which(colnames(m_polite_train)%in%c("First.Person.Single","Second.Person"))]
  m_test=m_polite_test[,-which(colnames(m_polite_test)%in%c("First.Person.Single","Second.Person")),drop=FALSE]

  recept_predict<-as.vector(politeness::politenessProjection(m_train,
                                                             train_dat$receptive,
                                                             m_test)$test_proj)


  return(recept_predict)
}
