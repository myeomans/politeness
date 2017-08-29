
library(politeness)
library(spacyr)
library(textir)
library(parallel)
library(quanteda)
library(ggplot2)
# upload data
df_train <- read.csv("./data/mnir_train_data.csv", stringsAsFactors = FALSE)



# make politness data frames
df_polite <- politeness(text = df_train$message, parser = "spacy", binary = TRUE)

# has bug
#df_polite_non_binary <- politeness(text = df_train$message, parser = "spacy", binary = FALSE)


df_train$warm_factor <- factor(df_train$warm)
levels(df_train$warm_factor) <- c("Cold","Warm")

df_train$warm_character <- as.character(df_train$warm)
df_train$warm_integer <- as.integer(df_train$warm)

# test logical
g_polite <- politenessPlot(df_polite,
                           split = df_train[["warm"]],
                           split_levels = c("not polite","polite"),
                           split_name =NULL,
                           top_title = ""
)

print(g_polite)


# test factor
g_polite <- politenessPlot(df_polite,
                           split = df_train[["warm_factor"]],
                           split_levels = c("not polite","polite"),
                           split_name =NULL,
                           top_title = ""
)

print(g_polite)

# test character
g_polite <- politenessPlot(df_polite,
                           split = df_train[["warm_character"]],
                           split_name =NULL,
                           top_title = ""
)

print(g_polite)

# test integer
g_polite <- politenessPlot(df_polite,
                           split = df_train[["warm_integer"]],
                           split_levels = c("not", ""),
                           split_name =NULL,
                           top_title = ""
)

print(g_polite)







