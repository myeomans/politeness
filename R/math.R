#' Variance-Weighted Log Odds
#' @description background function to load
#' @param x prevalence in one sample
#' @param y prevalence in another sample
#' @return variance-weighted log odds ratio of prevalence across samples
#' @keywords internal
slogodds <- function(x,y) {

  prior <- 1
  pcons <- (x+ prior)/sum(x + prior)
  plibs <- (y+ prior)/sum(y + prior)

  ocons <- pcons/(1-pcons)
  olibs <- plibs/(1-plibs)

  lor <- log(ocons) - log(olibs)
  sd.lor <-  sqrt(1/(x + prior) + 1/(y + prior))

  list(lor=lor, slor=(lor/sd.lor))
}
