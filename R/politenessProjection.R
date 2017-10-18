#' Politeness projection
#'
#' @description Projection of Multinomial Inverse Regression estimation for a politness matrix and covariates.
#' @param df_polite_train a data.frame with politeness features as outputed by \code{\link{politeness}} used to train model.
#' @param df_covar a data.frame with covariates.
#' @param df_polite_test  optional data.frame with politeness features as outputed by \code{\link{politeness}} used for model testing. Must have same feature set as polite_train (most easily acheived by setting \code{dropblank=FALSE} in both call to \code{politeness}).
#' @param classifier name of classification algorithm. Defaults to "mnir" (see \code{mnlm}) but "glmnet" (see \code{glmnet}) is also available.
#' @param ... additional parameters to be passed to the classification algorithm.
#' @return List of df_polite_train and df_polite_test with projection. See details.
#' @details List:
#' * train_proj matrix of projection of mlmn using df_covar and df_polite_train.
#' * test_proj matrix of projection of mlmn using df_covar and df_polite_train on df_polite_test data.
#' * train_coef coefficients of mnlm model using df_covar and df_polite_train.
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
#' project<-politenessProjection(polite.data,
#'                               phone_offers$condition,
#'                               polite.holdout)
#'
#' # Difference in average politeness across conditions in the new sample.
#'
#' mean(project$test_proj[bowl_offers$condition==1])
#' mean(project$test_proj[bowl_offers$condition==0])
#'
#' @export


politenessProjection <- function(df_polite_train, df_covar = NULL, df_polite_test = NULL, classifier = c("glmnet","mnir"),  ...){

  if(!is.null(df_covar)){
    # check that all colums of df_covar are numeric or logical
    v_s_not_num_or_logical <- names(df_covar)[ ! sapply(df_covar, function(col) is.numeric(col) | is.logical(col)) ]
    if(length(v_s_not_num_or_logical)>0 ){
      stop( paste0("All variables in df_covar must be logical or numeric. \n See following variables ",
                   paste0(v_s_not_num_or_logical, collapse = ", ")) )
    }

    if(!is.null(df_polite_test)){
      # check that df_polite_train and df_polite_test have the same columns
      if( !setequal(names(df_polite_train), names(df_polite_test)) ){
        stop("There must be the same variables in df_polite_train and df_polite_test")
      }
    }
    if (classifier[1] == "mnir"){
      m_polite_train <- as.matrix(df_polite_train)
      mnlm_fit <- suppressWarnings(textir::mnlm(cluster=NULL,
                                                covars = df_covar,
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
      polite_model<-glmnet::cv.glmnet(x=m_polite_train, y=df_covar, family="binomial", ...)
      polite_fit<-stats::predict(polite_model, newx=m_polite_train, s="lambda.1se", type="response")

      p_coefs<-as.matrix(stats::coef(polite_model, s="lambda.min"))
      polite_coefs<-p_coefs[(!(rownames(p_coefs)=="(Intercept)"))&p_coefs!=0,]

      if(!is.null(df_polite_test)){
        m_polite_test <- as.matrix(df_polite_test)
        polite_predict<-stats::predict(polite_model, newx=m_polite_test, s="lambda.min", type="response")
      } else {
        polite_predict <- NULL
      }
      l_out <- list(train_proj = polite_fit,
                    test_proj = polite_predict,
                    train_coefs = polite_coefs)

    }
  } else {
    stop("Function is not implemented for NULL df_covar")
  }

  return(l_out)

}
