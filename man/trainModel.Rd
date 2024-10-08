% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trainModel.R
\name{trainModel}
\alias{trainModel}
\title{Train a model with politeness features}
\usage{
trainModel(
  df_polite_train,
  covar = NULL,
  df_polite_test = NULL,
  classifier = c("glmnet", "mnir"),
  cv_folds = NULL,
  ...
)
}
\arguments{
\item{df_polite_train}{a data.frame with politeness features as outputed by \code{\link{politeness}} used to train model.}

\item{covar}{a vector of politeness labels, or other covariate.}

\item{df_polite_test}{optional data.frame with politeness features as outputed by \code{\link{politeness}} used for out-of-sample fitting. Must have same feature set as polite_train (most easily achieved by setting \code{dropblank=FALSE} in both calls to \code{politeness}).}

\item{classifier}{name of classification algorithm. Defaults to "glmnet" (see \code{glmnet}) but "mnir" (see \code{mnlm}) is also available.}

\item{cv_folds}{Number of outer folds for projection of training data. Default is NULL (i.e. no nested cross-validation). However, positive values are highly recommended (e.g. 10) for in-sample accuracy estimation.}

\item{...}{additional parameters to be passed to the classification algorithm.}
}
\value{
List of df_polite_train and df_polite_test with projection. See details.
}
\description{
Training and projecting a regression model using politeness features.
}
\details{
List:
\itemize{
\item train_proj projection of politeness model within training set.
\item test_proj projection of politeness model onto test set (i.e. out-of-sample).
\item train_coef coefficients from the trained model.
\item train_model The LASSO model itself (for modelPlot)
}
}
\examples{

data("phone_offers")
data("bowl_offers")

polite.data<-politeness(phone_offers$message, parser="none",drop_blank=FALSE)

polite.holdout<-politeness(bowl_offers$message, parser="none",drop_blank=FALSE)

project<-trainModel(polite.data,
                              phone_offers$condition,
                              polite.holdout)

# Difference in average politeness across conditions in the new sample.

mean(project$test_proj[bowl_offers$condition==1])
mean(project$test_proj[bowl_offers$condition==0])

}
