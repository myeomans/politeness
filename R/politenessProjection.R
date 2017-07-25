

#' Politeness projection
#' @description Projection of Multinomial Inverse regression estimation for a politness matrix and covariates
#' @param df_polite a data.frame with politeness features as outputed by \code{politeness} used to train model.
#' @param df_polite a data.frame with politeness features as outputed by \code{politeness} used for model testing.
#' @param df_covar a data.frame with covariates.
#' @param mnlm_cluster cluster to be used in \code{mnlm} see  \code{mnlm} and \code{makeCluster}.
#' @param ... additional parameters to be passed to \code{mnlm}.
#' @output List of df_polite_train and df_polite_test with projection.
#' @details detailed explanation 

politenessProjection <- function(df_polite_train, df_covar = NULL, df_polite_test = NULL, mnlm_cluster = NULL, ...){
  if(!is.null(df_covar)){
    # check that all colums of df_covar are numeric
    stopifnot( sapply(df_covar, is.numeric))
    
    m_polite_train <- as.matrix(df_polite_train)
    mnlm_fit <- mnlm(mnlm_cluster, 
                     covars = df_covar, 
                     counts = m_polite_train, ...)
    mnlm_coeficients = coef(fits)
    
    m_train_proj <- srproj(mnlm_fit, m_polite_train )
    
    if(!is.null(df_polite_test)){
      m_polite_test <- as.matrix(df_polite_test)
      m_test_proj <- srproj(mnlm_fit, m_polite_test )
    } else{
      m_test_proj <- NULL
    }
    
    l_out <- list(train_proj = m_train_proj, test_proj = m_test_proj)
    
  } else {
    stop("Function is not implemented for NULL df_covar")
  }
  
  return(l_out)
  
}