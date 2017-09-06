#' Politeness projection
#'
#' @description Projection of Multinomial Inverse Regression estimation for a politness matrix and covariates.
#' @param df_polite_train a data.frame with politeness features as outputed by \code{\link{politeness}} used to train model.
#' @param df_covar a data.frame with covariates.
#' @param df_polite_test  optional data.frame with politeness features as outputed by \code{\link{politeness}} used for model testing. Must have same feature set as polite_train (most easily acheived by setting \code{dropblank=FALSE} in both call to \code{politeness}).
#' @param mnlm_cluster cluster to be used in \code{mnlm}. See  \code{mnlm} and \code{makeCluster}.
#' @param ... additional parameters to be passed to \code{mnlm}.
#' @return List of df_polite_train and df_polite_test with projection. See details.
#' @details List:
#' * train_proj matrix of projection of mlmn using df_covar and df_polite_train.
#' * test_proj matrix of projection of mlmn using df_covar and df_polite_train on df_polite_test data.
#' * train_coef coefficients of mnlm model using df_covar and df_polite_train.
#' @md
#' @examples
#'
#'

politenessProjection <- function(df_polite_train, df_covar = NULL, df_polite_test = NULL, mnlm_cluster = NULL, ...){
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

    m_polite_train <- as.matrix(df_polite_train)
    mnlm_fit <- textir::mnlm(mnlm_cluster,
                             covars = df_covar,
                             counts = m_polite_train, ...)
    mnlm_coef = coef(mnlm_fit)

    m_train_proj <- textir::srproj(mnlm_fit, m_polite_train )

    if(!is.null(df_polite_test)){
      m_polite_test <- as.matrix(df_polite_test)
      m_test_proj <- textir::srproj(mnlm_fit, m_polite_test )
    } else{
      m_test_proj <- NULL
    }

    l_out <- list(train_proj = m_train_proj, test_proj = m_test_proj, train_coefs=mnlm_coef)

  } else {
    stop("Function is not implemented for NULL df_covar")
  }

  return(l_out)

}
