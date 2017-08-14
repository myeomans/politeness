#' Find Polite text
#' @description Finds examples of most or least polite text in a corpus
#' @param
#' @param
#' @param
#' @return
#' @details
#' @examples
#'
#'

findPoliteTexts <- function(df_polite,
                                df_covar,
                                text,
                                type = c("most","least","both"),
                                num_docs = 5L,
                                mnlm_cluster = NULL,
                                ...){
  # check that df_polite, df_covar, and text have same number of 'rows'

  # check type
  valid_type <- c("most","least","both")
  type <- type[1] # in case type has length > 1 only use first entry
  if( ! type %in% valid_type){
    stop( paste0("type must be one of the following ", paste0(valid_type, collapse = ", ")))
  }

  l_proj <- politenessProjection(df_polite_train = df_polite,
                                 df_covar = df_covar,
                                 mnlm_cluster = mnlm_cluster,
                                 ... )

  m_train_proj <- l_polite_proj$train_proj
  df_docs_proj <- data.frame(text = text, projection = m_train_proj[ , 1])

  if(type %in% c("most","least")){
    is_most <-  type == "most"
    df_docs_proj <- df_docs_proj[ order(df_docs_proj$projection, decreasing = is_most) , ]
    df_out <- df_docs_proj[ 1:num_docs , "text", drop = FALSE ]
    df_out$rank <-  1:num_docs
  } else {
    df_out <- df_out[ order(df_out$projection, decreasing = TRUE) , ]

    num_docs_sub <- num_docs %/% 2
    # check if num_docs is even
    num_most <- ifelse( num_docs %% 2 == 0 , num_docs_sub, num_docs_sub + 1    )
    num_least <- num_docs_sub

    df_docs_proj <- df_docs_proj[ order(df_docs_proj$projection, decreasing = TRUE) , ]
    df_most_polite <- df_docs_proj[ 1:num_most , "text", drop = FALSE ]
    df_most$rank <-  1:num_most

    df_docs_proj <- df_docs_proj[ order(df_docs_proj$projection, decreasing = FALSE) , ]
    df_least_polite <- df_docs_proj[ 1:num_least , "text", drop = FALSE ]
    df_least_polite$rank <-  1:num_least

    df_out <- rbind(df_most_polite, df_least_polite)
  }
  return(df_out)
}
