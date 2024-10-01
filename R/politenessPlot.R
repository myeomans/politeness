#' Politeness plot
#'
#' @description Deprecated... This function has a new name now. See featurePlot for details.
#' @param df_polite a data.frame with politeness features calculated from a document set, as output by \code{\link{politeness}}.
#' @param ... other arguments passed on to featurePlot. See featurePlot for details.
#' @return a ggplot of the prevalence of politeness features, conditional on \code{split}. Features are sorted by variance-weighted log odds ratio.
#' @examples
#'
#' data("phone_offers")
#'
#' polite.data<-politeness(phone_offers$message, parser="none", drop_blank=FALSE)
#'
#' politeness::politenessPlot(polite.data,
#'                            split=phone_offers$condition,
#'                            split_levels = c("Tough","Warm"),
#'                            split_name = "Condition",
#'                            top_title = "Average Feature Counts")
#'
#' @export

politenessPlot<-function(df_polite,...){
  featurePlot(df_polite,...)
}
