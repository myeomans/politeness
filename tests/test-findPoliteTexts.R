
library(politeness)
library(spacyr)
library(textir)
library(parallel)
library(quanteda)

# upload data
df_train <- read.csv("./data/mnir_train_data.csv", stringsAsFactors = FALSE)
df_test <- read.csv("./data/mnir_test_data.csv", stringsAsFactors = FALSE)

# make politness data frames
df_politeness_train <- politeness(text = df_train$message, parser = "spacy", binary = TRUE)
df_politeness_test <- politeness(text = df_test$message, parser = "spacy", binary = TRUE)

df_polite <- findPoliteTexts(text = df_train$message,
                             df_polite = df_politeness_train,
                             df_covar = df_train[ , "warm" , drop = FALSE] )



df_polite <- findPoliteTexts(text =  df_test$message,
                             df_polite = df_politeness_test,
                             df_covar = df_test[ , "warm" , drop = FALSE] ,
                             type = "both",
                             num_docs = 9)

