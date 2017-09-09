#
# library(politeness)
# library(spacyr)
# library(textir)
# library(parallel)
# library(quanteda)
# library(ROCR)
#
# calcAUROC <- function( v_estimated, v_observed){
#   pred <- prediction(v_estimated,  v_observed)
#   auc_tmp <- performance(pred,"auc")
#   auc <- as.numeric(auc_tmp@y.values)
#   return(auc)
# }
# testingPolitenessProjection <- function(df_politeness_train, df_politeness_test, df_train,df_test){
#   l_polite_proj <- politenessProjection(df_polite_train = df_politeness_train,
#                                         df_covar = df_train[ , "warm" , drop = FALSE],
#                                         df_polite_test = df_politeness_test)
#
#   m_train_proj <- l_polite_proj$train_proj
#   m_test_proj <- l_polite_proj$test_proj
#   glm_proj <- glm( df_train$warm ~ m_train_proj, family = binomial())
#   print("Auroc train")
#   print(calcAUROC(predict(glm_proj, type = "response"),  df_train$warm))
#
#   glm_proj <- glm( df_test$warm ~ m_test_proj, family = binomial())
#   print("Auroc test")
#   print(calcAUROC(predict(glm_proj, type = "response"),  df_test$warm))
#
# }
#
# # upload data
# df_train <- read.csv("./data/mnir_train_data.csv", stringsAsFactors = FALSE)
# df_test <- read.csv("./data/mnir_test_data.csv", stringsAsFactors = FALSE)
#
# # make politness data frames
# df_politeness_train <- politeness(text = df_train$message, parser = "spacy", binary = TRUE)
# df_politeness_test <- politeness(text = df_test$message, parser = "spacy", binary = TRUE)
#
# # error!
# # calling politenessProjection produces error because df_politeness_train and df_politeness_test have diferent columns
# testingPolitenessProjection(df_politeness_train, df_politeness_test, df_train,df_test)
#
# # correcting for different columns
# v_s_shared_columns <- intersect(names(df_politeness_train),names(df_politeness_test))
# df_politeness_train <- df_politeness_train[ , v_s_shared_columns]
# df_politeness_test <- df_politeness_test[ , v_s_shared_columns]
#
# # not it runs with good auroc
# testingPolitenessProjection(df_politeness_train, df_politeness_test, df_train,df_test)
#
# # run with binary false
# df_politeness_train <- politeness(text = df_train$message, parser = "spacy", binary = FALSE)
# df_politeness_test <- politeness(text = df_test$message, parser = "spacy", binary = FALSE)
# v_s_shared_columns <- intersect(names(df_politeness_train),names(df_politeness_test))
# df_politeness_train <- df_politeness_train[ , v_s_shared_columns]
# df_politeness_test <- df_politeness_test[ , v_s_shared_columns]
#
# testingPolitenessProjection(df_politeness_train, df_politeness_test, df_train,df_test)
#
# # run with drop.blank false
# df_politeness_train <- politeness(text = df_train$message, parser = "spacy", binary = FALSE,drop.blank = FALSE)
# df_politeness_test <- politeness(text = df_test$message, parser = "spacy", binary = FALSE, drop.blank = FALSE)
# testingPolitenessProjection(df_politeness_train, df_politeness_test, df_train,df_test)
#
#
