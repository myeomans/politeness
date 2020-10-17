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

#' Fold Assignment for Cross-Validation
#' @description background function to load
#' @param sizer number of observations in dataset.
#' @param nfold number of outer folds needed.
#' @param balance Optional vector of a categorical covariate to stratify fold assignment
#' @return vector of fold IDs
#' @keywords internal
foldset<-function (sizer, nfold, balance = NA) {

  folds <- sample(rep(1:nfold, ceiling(sizer/nfold))[1:sizer])
  if (!is.na(balance[1])) {
    if ((length(balance) == sizer)) {
      folds <- rep(NA, sizer)
      for (u in unique(balance)) {
        setID <- which(balance == u)
        setfold <- sample(rep(1:nfold, ceiling(length(setID)/nfold))[1:length(setID)])
        folds[setID] <- setfold
      }
    }
    else "error: balance length does not match"
  }
  return(folds)
}

