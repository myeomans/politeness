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
#' @format A list of 99 hedging words.
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
#' @source Jeong, M., Minson, J., Yeomans, M. & Gino, F. (working paper).
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
#' @source Jeong, M., Minson, J., Yeomans, M. & Gino, F. (working paper).
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
