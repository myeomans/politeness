
# upload data

data("phone_offers")




# make politness data frames
df_polite <- politeness(text = phone_offers$message, parser = "none", binary = TRUE)

# has bug
#df_polite_non_binary <- politeness(text = df_train$message, parser = "spacy", binary = FALSE)


phone_offers$condition_factor <- factor(phone_offers$condition)
levels(phone_offers$condition_factor) <- c("Cold","Warm")

phone_offers$condition_character <- as.character(phone_offers$condition_factor)
phone_offers$condition_integer <- as.integer(phone_offers$condition)
phone_offers$condition_logical <- as.logical(phone_offers$condition)
View(phone_offers)
# test logical
g_polite <- politenessPlot(df_polite,
                           split = phone_offers[["condition_logical"]],
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







