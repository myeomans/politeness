utils::globalVariables(c("polite_train")) # prevent incorrect "no global binding" note

#' Pre-Trained Politeness Classifier
#'
#' @description Pre-trained model to detect conversational receptiveness
#' @param texts character A vector of texts, each of which will be tallied for politeness features.
#' @param num_mc_cores integer Number of cores for parallelization.
#' @details This is a wrapper around a pre-trained model of "politeness" for all the data from the 2013 DNM et al paper.
#' This model requires grammar parsing via SpaCy. Please see \code{\link{spacyr}} for details on installation.
#' @return a vector with receptiveness scores
#' @references
#' Danescu-Niculescu-Mizil, C., Sudhof, M., Jurafsky, D., Leskovec, J. & Potts, C. (2013). A computational approach to politeness with application to social factors. Proc. 51st ACL, 250-259.
#'
#' @examples
#'
#'
#'\dontrun{
#' data("phone_offers")
#'
#' politenessModel(phone_offers$message)
#'
#'}
#'
#'@export
politenessModel<-function(texts, num_mc_cores=1){
  m_polite_train = as.matrix(politeness::polite_train$x)

  m_polite_test = as.matrix(politeness::politeness(texts,
                                                   parser="spacy",
                                                   num_mc_cores=num_mc_cores))

  # Remove 1st/2nd person pronouns
  m_train=m_polite_train[,-which(colnames(m_polite_train)%in%c("First.Person.Single","Second.Person"))]
  m_test=m_polite_test[,-which(colnames(m_polite_test)%in%c("First.Person.Single","Second.Person")),drop=FALSE]

  recept_predict<-as.vector(politeness::politenessProjection(m_train,
                                                             politeness::polite_train$y,
                                                             m_test)$test_proj)


  return(recept_predict)
}
