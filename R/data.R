#' #' Positive Emotions List
#' #'
#' #' Positive words.
#' #'
#' #' @format A list of 2006 positively-valenced words
#' #'
#' "positive_list"
#'
#' #' Negative Emotions List
#' #'
#' #' Negative words.
#' #'
#' #' @format A list of 4783 negatively-valenced words
#' #'
#' "negative_list"
#'
#' #' Hedge Words List
#' #'
#' #' Hedges
#' #'
#' #' @format A list of 72 hedging words.
#' #'
#' "hedge_list"
#'
#' #' Feature Dictionaries
#' #'
#' #' Six dictionary-like features for the detector: Negations; Pauses; Swearing; Pronouns; Formal Titles; and Informal Titles.
#' #'
#' #' @format A list of six \code{quanteda::dictionary} objects
#' "polite_dicts"

#' Purchase offers for phone
#'
#' A dataset containing the purchase offer message and a
#' label indicating if the writer was assigned to be warm (1) or tough (0)
#' @format A data frame with 355 rows and 2 variables:
#' \describe{
#'   \item{message}{character of purchase offer message}
#'   \item{condition}{binary label indicating if message is warm or tough}
#' }
#' @source Jeong, M., Minson, J., Yeomans, M. & Gino, F. (2019).
#'
#' "Communicating Warmth in Distributed Negotiations is Surprisingly Ineffective."
#'
#' Study 1. \url{https://osf.io/t7sd6/}
"phone_offers"


#' Purchase offers for bowl
#'
#' A dataset containing the purchase offer message and a
#' label indicating if the writer was assigned to be warm (1) or tough (0)
#' @format A data frame with 70 rows and 2 variables:
#' \describe{
#'   \item{message}{character of purchase offer message}
#'   \item{condition}{binary label indicating if message is warm or tough}
#' }
#' @source Jeong, M., Minson, J., Yeomans, M. & Gino, F. (2019).
#'
#' "Communicating Warmth in Distributed Negotiations is Surprisingly Ineffective." Study 3.
#'
#' Study 3. \url{https://osf.io/t7sd6/}
"bowl_offers"


#' Table of Politeness Features
#'
#' This table describes all the text features extracted in this package. See vignette for details.
#'
#' @format A data.frame with information about the politeness features.
#'
"feature_table"



#' Pre-Trained Receptiveness Data
#'
#' A dataset to train a model for detecting conversational receptiveness.
#' @format A data frame with 2860 rows and 2 variables:
#' \describe{
#'   \item{text}{character written response about policy disagreement}
#'   \item{receptive}{numeric standardized average of annotator ratings for "receptiveness"}
#' }
#' Primarily for use within the receptiveness() function. The data was compiled from
#' Studies 1 and 4 of the original paper, as well as an unpublished study with a
#' very similar design, in which text responses were rated by disagreeing others.
#' @source  Yeomans, M., Minson, J., Collins, H., Chen, F. & Gino, F. (2020).
#'
#' "Conversational Receptiveness: Improving Engagement with Opposing Views"
#'
#' \url{https://osf.io/2n59b/}
#'
"receptive_train"


#' Pre-Trained Receptiveness Data
#'
#' A dataset to train a model for detecting conversational receptiveness.
#' @format Pre-calculated politeness features for the receptive_train dataset
#'
"receptive_polite"

#' Pre-Trained Politeness
#'
#' A dataset to train a model for detecting politeness.
#'
#' @source Danescu-Niculescu-Mizil, C., Sudhof, M., Jurafsky, D., Leskovec, J. & Potts, C. (2013). A computational approach to politeness with application to social factors. Proc. 51st ACL, 250-259.
#'
#' @format list of two objects. x contains pre-calculated politeness features for each document. y contains standardized human annotations for politeness.
#'
"polite_train"


#' UK to US Conversion dictionary
#'
#' For internal use only. This dataset contains a quanteda dictionary for converting UK words to US words. The models in this package were all trained on US English.
#'
#' @format A quanteda dictionary with named entries. Names are the US version, and entries are the UK version.
#' @source Borrowed from the quanteda.dictionaries package on github (from user kbenoit)
#'
"uk2us"

#' A pre-trained model for detecting conversational receptiveness.
#' Estimated with glmnet using annotated data from a previous paper.
#' Primarily for use within the receptiveness() function.
#'
#' @format A fitted glmnet model
#' @source  Minson, J., Yeomans, M., Collins, H. & Dorison, C.
#'
#' "Conversational Receptiveness: Improving Engagement with Opposing Views"
#'
#'
#'
"receptive_model"



#' This is the list of variables to be extracted for the receptiveness algorithm
#' For internal use only, within the receptiveness() function.
#'
#' @format Character vector containing variable names
#' @source  Minson, J., Yeomans, M., Collins, H. & Dorison, C.
#'
#' "Conversational Receptiveness: Improving Engagement with Opposing Views"
#'
#'
#'
"receptive_names"
