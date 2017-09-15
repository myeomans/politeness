
data("phone_offers")
phone_offers <- phone_offers[1:10,]

context("politeness function")

num_messages <- length(phone_offers$message)
#
df_polite_bin_drop <- politeness(text = phone_offers$message, parser = "none", binary = TRUE, drop_blank = TRUE)

# binary false, drop_blank true
df_polite_bin_f_drop <- politeness(text = phone_offers$message, parser = "spacy", binary = FALSE, drop_blank = TRUE)

# binary false, drop_blank false
df_polite_bin_f_drop_f <- politeness(text = phone_offers$message, parser = "spacy", binary = FALSE, drop_blank = FALSE)

# binary true, drop_blank false
df_polite_bin_drop_f <- politeness(text = phone_offers$message, parser = "spacy", binary = TRUE, drop_blank = FALSE)

test_that("runs with parser none", {
  expect_equal(nrow(df_polite_bin_drop), num_messages)
  expect_true(setequal(unique(unlist(df_polite_bin_drop)),0:1))

  expect_equal(nrow(df_polite_bin_drop), num_messages)
  expect_true(setequal(unique(unlist(df_polite_bin_drop_f)),0:1))

  expect_equal(nrow(df_polite_bin_f_drop_f), num_messages)
  expect_false(setequal(unique(unlist(df_polite_bin_f_drop_f)),0:1))

  expect_equal(nrow(df_polite_bin_f_drop), num_messages)
  expect_false(setequal(unique(unlist(df_polite_bin_f_drop)),0:1))
})


phone_offers$message[1] <- ""

df_polite_bin_drop <- politeness(text = phone_offers$message, parser = "none", binary = TRUE, drop_blank = TRUE)


#! should this through an error??
phone_offers$message[1] <- NA_character_

df_polite_bin_drop <- politeness(text = phone_offers$message, parser = "none", binary = TRUE, drop_blank = TRUE)


# what if data has NA?
#" what if fhta is empty"
#


# spacy
# binary true, drop.blank true






