
data("phone_offers")
phone_offers <- phone_offers[1:10,]

context("politeness function")

num_messages <- length(phone_offers$message)
#
df_polite_bin_drop <- politeness(text = phone_offers$message, parser = "none", binary = TRUE, drop_blank = TRUE)

# binary false, drop_blank true
df_polite_bin_f_drop <- politeness(text = phone_offers$message, parser = "none", binary = FALSE, drop_blank = TRUE)

# binary false, drop_blank false
df_polite_bin_f_drop_f <- politeness(text = phone_offers$message, parser = "none", binary = FALSE, drop_blank = FALSE)

# binary true, drop_blank false
df_polite_bin_drop_f <- politeness(text = phone_offers$message, parser = "none", binary = TRUE, drop_blank = FALSE)

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

test_that("empty or na string", {
 phone_offers$message[1] <- ""

 df_polite <- politeness(text = c("","a"), parser = "none", binary = TRUE, drop_blank = FALSE)
 expect_equal(apply(df_polite, MARGIN = 1, FUN  = sum), c(0,0))

 df_polite <- politeness(text = c(NA_character_,"a"), parser = "none", binary = TRUE, drop_blank = FALSE)
 expect_equal(apply(df_polite, MARGIN = 1, FUN  = sum), c(0,0))

 phone_offers$message[1] <- NA_character_

 df_polite <- politeness(text = phone_offers$message, parser = "none", binary = TRUE, drop_blank = FALSE)
 expect_equal(apply(df_polite, MARGIN = 1, FUN  = sum)[1], 0)
})


test_that("factor", {
  phone_offers$message[1] <- ""
  phone_offers$message <- factor(phone_offers$message)
  df_polite <- politeness(text = phone_offers$message, parser = "none", binary = TRUE, drop_blank = FALSE)
  expect_true(any(apply(df_polite, MARGIN = 1, FUN  = sum) > 0))
})

test_that("text of length 0", {
  df_polite <- politeness(text = phone_offers$message[2], parser = "none", binary = TRUE, drop_blank = FALSE)
  expect_true(apply(df_polite, MARGIN = 1, FUN  = sum) > 0)
})






