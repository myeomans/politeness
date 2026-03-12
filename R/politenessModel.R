utils::globalVariables(c("polite_train")) # prevent incorrect "no global binding" note

#' Pre-Trained Politeness Classifier
#'
#' @description Pre-trained model to detect politeness based on data from Danescu-Niculescu-Mizil et al. (2013)
#' @param texts character A vector of texts, each of which will be given a politeness score.
#' @param num_mc_cores integer Number of cores for parallelization.
#' @details This is a wrapper around a pre-trained model of "politeness" for all the data from the 2013 DNM et al paper.
#' This model requires grammar parsing via SpaCy. Please see \code{\link[spacyr:spacyr]{spacyr}} for details on installation.
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

  m_polite_test = as.matrix(politeness::politenessDNM(texts))

  recept_predict<-as.vector(politeness::trainModel(m_polite_train,
                                                   politeness::polite_train$y,
                                                   m_polite_test)$test_proj)


  return(recept_predict)
}

# ################################################
# Features that are more or less the same (some different names)
#
# #
# # 1. Gratitude
# # 6. Apologizing
# # 2. Deference (affirmation)
# # 3. Greeting ( hello)
# # 5. Factuality
# # 9. Indirect (btw)
# # 11. Direct start (conj start)
# # 12. Counterfactual modal (Could/Would you)
# # 13. Indicative modal (Can/Will you)
# # 15. 1st person plural
#
# "Gratitude"
# "Apology"
# "Affirmation"
# "Hello"
# "Truth.Intensifier"
# "By.The.Way"
# "Conjunction.Start"
# "Could.You"
# "Can.You"
# "First.Person.Plural"
#
# ################################################
# # Similar feature, different definition
#
# # These two did not have negation scoping
# # 4. Positive
# # 5. Negative
# "Positive.Emotion"
# "Negative.Emotion"
#
# # This used to include a lot of subjectivity as well
# # 4. Hedges (old hedges)
# "Hedges"
#
# # This was just one category
# # 10. Direct question
# #why = lambda p: (getleftpos(p) in (1,2) and getleft(p) in ("what","why","who","how")) or (getrightpos(p) in (1,2) and getright(p) in ("what","why","who","how"))
# #why.__name__ = "Direct question"
# "WH.Questions"
# "YesNo.Questions"
#
# # For these, they were originally split into "start" (first token) vs. generic, and we combined them
# # 7. Please
# # 8. Please start
# # 16. 1st person
# # 14. 1st person start
# # 17. 2nd person
# # 18. 2nd person start
# "Please"
# "First.Person.Single"
# "Second.Person"
#
#
# ################################################
# # Not in the original paper at all
# #
# # "Impersonal.Pronoun"
# # "Swearing"
# # "Negation"
# # "Filler.Pause"
# # "Informal.Title"
# # "Formal.Title"
# # "Let.Me.Know"
# # "Goodbye"
# # "For.Me"
# # "For.You"
# # "Reasoning"
# # "Reassurance"
# # "Ask.Agency"
# # "Give.Agency"
# # "Agreement"
# # "Disagreement"
# # "Acknowledgement"
# # "Subjectivity"
# # "Bare.Command"
# # "Adverb.Limiter"
#


