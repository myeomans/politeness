
library(politeness)
library(spacyr)
library(textir)
library(parallel)
library(quanteda)
# upload data
df_train <- read.csv("./data/mnir_train_data.csv", stringsAsFactors = FALSE)


# spacy
# binary true, drop.blank true
df_polite_bin_drop <- politeness(text = df_train$message, parser = "spacy", binary = TRUE, drop.blank = TRUE)


# binary false, drop.blank true
df_polite_bin_f_drop <- politeness(text = df_train$message, parser = "spacy", binary = FALSE, drop.blank = TRUE)


# binary false, drop.blank false
df_polite_bin_f_drop_f <- politeness(text = df_train$message, parser = "spacy", binary = FALSE, drop.blank = FALSE)


# binary true, drop.blank false
df_polite_bin_drop_f <- politeness(text = df_train$message, parser = "spacy", binary = TRUE, drop.blank = FALSE)


