#' Positive Emotions List
#'
#' Positive words.
#'
#' @format A list of 2006 positively-valenced words
#'
"positive_list"

#' Negative Emotions List
#'
#' Negative words.
#'
#' @format A list of 4783 negatively-valenced words
#'
"negative_list"

#' Hedge Words List
#'
#' Hedges
#'
#' @format A list of 72 hedging words.
#'
"hedge_list"

#' Feature Dictionaries
#'
#' Six dictionary-like features for the detector: Negations; Pauses; Swearing; Pronouns; Formal Titles; and Informal Titles.
#'
#' @format A list of six \code{quanteda::dictionary} objects
"polite_dicts"

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
#' @format A data.frame with information about 36 politeness features.
#'
"feature_table"



#' Pre-Trained Receptiveness Data
#'
#' A dataset to train a model for detecting conversational receptiveness.
#' @format A data frame with 543 rows and 2 variables:
#' \describe{
#'   \item{text}{character written response about policy disagreement}
#'   \item{receptive}{numeric average of annotator ratings for "receptiveness"}
#' }
#' Primarily for use within the receptiveness() function.
#' @source  Yeomans, M., Minson, J., Collins, H., Chen, F. & Gino, F. (2020).
#'
#' "Conversational Receptiveness: Improving Engagement with Opposing Views"
#'
#' Study 1. \url{https://osf.io/2n59b/}
#'
#'
"receptive_train"


#' UK to US Conversion dictionary
#'
#' For internal use only. This dataset contains a quanteda dictionary for converting UK words to US words. The models in this package were all trained on US English.
#'
#' @format A quanteda dictionary with named entries. Names are the US version, and entries are the UK version.
#' @source Borrowed from the quanteda.dictionaries package on github (from user kbenoit)
#'
"uk2us"

