#' Train a model with politeness features
#'
#' @description Training and projecting a regression model using politeness features.
#' @param df_polite_train a data.frame with politeness features as outputed by \code{\link{politeness}} used to train model.
#' @param covar a vector of politeness labels, or other covariate.
#' @param df_polite_test  optional data.frame with politeness features as outputed by \code{\link{politeness}} used for out-of-sample fitting. Must have same feature set as polite_train (most easily achieved by setting \code{dropblank=FALSE} in both calls to \code{politeness}).
#' @param classifier name of classification algorithm. Defaults to "glmnet" (see \code{glmnet}) but "mnir" (see \code{mnlm}) is also available.
#' @param cv_folds Number of outer folds for projection of training data. Default is NULL (i.e. no nested cross-validation). However, positive values are highly recommended (e.g. 10) for in-sample accuracy estimation.
#' @param ... additional parameters to be passed to the classification algorithm.
#' @return List of df_polite_train and df_polite_test with projection. See details.
#' @details List:
#' * train_proj projection of politeness model within training set.
#' * test_proj projection of politeness model onto test set (i.e. out-of-sample).
#' * train_coef coefficients from the trained model.
#' * train_model The LASSO model itself (for modelPlot)
#'
#' @md
#' @examples
#'
#' data("phone_offers")
#' data("bowl_offers")
#'
#' polite.data<-politeness(phone_offers$message, parser="none",drop_blank=FALSE)
#'
#' polite.holdout<-politeness(bowl_offers$message, parser="none",drop_blank=FALSE)
#'
#' project<-trainModel(polite.data,
#'                               phone_offers$condition,
#'                               polite.holdout)
#'
#' # Difference in average politeness across conditions in the new sample.
#'
#' mean(project$test_proj[bowl_offers$condition==1])
#' mean(project$test_proj[bowl_offers$condition==0])
#'
#' @import glmnet
#' @export


trainModel <- function(df_polite_train, covar = NULL,
                       df_polite_test = NULL,
                       classifier = c("glmnet","mnir"),
                       cv_folds=NULL, ...){
  if(is.null(covar)){
    stop("Must supply covariate to train model")
  }
  if(sum(is.na(covar))>0){
    stop("Covariate may not have NA values")
  }

  # check that all elements of covar are numeric or logical
  v_s_not_num_or_logical <- unlist(sapply(covar, function(col) !(is.numeric(col) | is.logical(col))))
  if(sum(v_s_not_num_or_logical)>0 ){
    stop( paste0("covar must be logical or numeric.") )
  }

  if(!is.null(df_polite_test)){
    # check that df_polite_train and df_polite_test have the same columns
    if( !setequal(names(df_polite_train), names(df_polite_test)) ){
      stop("There must be the same variables in df_polite_train and df_polite_test")
    }
  }

  if (classifier[1] == "mnir"){
    m_polite_train <- as.matrix(df_polite_train)
    mnlm_fit <- suppressWarnings(textir::mnlm(cl=NULL,
                                              covars = covar,
                                              counts = m_polite_train,
                                              ...))
    mnlm_coef = suppressWarnings(stats::coef(mnlm_fit))

    m_train_proj <- suppressWarnings(textir::srproj(mnlm_fit, m_polite_train ))

    if(!is.null(df_polite_test)){
      m_polite_test <- as.matrix(df_polite_test)
      m_test_proj <- textir::srproj(mnlm_fit, m_polite_test )
    } else{
      m_test_proj <- NULL
    }

    l_out <- list(train_proj = m_train_proj[,1], test_proj = m_test_proj[,1], train_coefs=mnlm_coef[2,])
  }
  if (classifier[1] == "glmnet"){
    m_polite_train <- as.matrix(df_polite_train)

    if( is.data.frame(covar)){
      #take first variable
      covar <- covar[[1]]
    }

    # chose family based on covar
    is_binary <- length(unique(covar)) == 2
    model_family <- ifelse(is_binary, "binomial", "gaussian")

    # Nested cross-validation for in-sample accuracy estimation
    if(is.numeric(cv_folds)){
      foldIDs<-foldset(length(covar), cv_folds)
      tpb<-utils::txtProgressBar(0,cv_folds)

      polite_fit<-rep(NA,cv_folds)

      for(fold in 1:max(foldIDs)){
        train.fold<-(foldIDs!=fold)
        test.fold<-(foldIDs==fold)

        polite_model_fold<-glmnet::cv.glmnet(x=m_polite_train[train.fold,],
                                             y=covar[train.fold],
                                             family=model_family, ...)

        polite_fit[test.fold]<-as.vector(stats::predict(polite_model_fold,
                                                        s="lambda.min",
                                                        newx=m_polite_train[test.fold,],
                                                        type="response"))
        utils::setTxtProgressBar(tpb,fold)
      }

      polite_model<-glmnet::cv.glmnet(x=m_polite_train, y=covar, family=model_family, ...)
      p_coefs<-as.matrix(stats::coef(polite_model))
      polite_coefs<-p_coefs[(!(rownames(p_coefs)=="(Intercept)"))&p_coefs!=0,]


    } else{
      # Fitted values with no cross-validation
      if(is.null(df_polite_test)){
        warning("Note: no cross-validation. Projections in training data are not suitable for accuracy estimation.")
      }
      polite_model<-glmnet::cv.glmnet(x=m_polite_train, y=covar, family=model_family, ...)
      polite_fit<-stats::predict(polite_model, newx=m_polite_train,
                                 s="lambda.min",
                                 type="response")[,1]

      p_coefs<-as.matrix(stats::coef(polite_model,s="lambda.min"))
      polite_coefs<-p_coefs[(!(rownames(p_coefs)=="(Intercept)"))&p_coefs!=0,]

    }

    if(!is.null(df_polite_test)){
      m_polite_test <- as.matrix(df_polite_test)
      polite_predict<-stats::predict(polite_model, newx=m_polite_test, type="response")[,1]
    } else {
      polite_predict <- NULL
    }
    l_out <- list(train_proj = polite_fit,
                  test_proj = polite_predict,
                  train_coefs = polite_coefs,
                  train_model = polite_model)

  }

  return(l_out)

}
