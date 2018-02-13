data("phone_offers")
df_polite <-politeness(phone_offers$message, parser="none",drop_blank=FALSE, num_mc_cores=1)

df_most <- suppressWarnings(findPoliteTexts(phone_offers$message,
                                            df_polite = df_polite,
                                            phone_offers$condition,
                                            num_docs = 10))
df_both <- suppressWarnings(findPoliteTexts(phone_offers$message,
                                            type = "both",
                                            df_polite = df_polite,
                                            phone_offers$condition,
                                            num_docs = 13))


test_that("find polite correct dimensions",{
  expect_equal(nrow(df_most) , 10)
  expect_equal(nrow(df_both) , 13)
  expect_equal(colnames(df_both), c("text","rank","group"))
})

test_that("find polite in original data",{
  expect_true(all(df_most$text %in% phone_offers$message))
})

num_obs <- length(phone_offers$condition)
set.seed(100)
continous_condition <- ifelse(as.logical(phone_offers$condition),
                              stats::rnorm(num_obs,mean = 5),
                              stats::rnorm(num_obs, mean = 1))

test_that("find polite runs with different parameter" , {
  expect_that({
    suppressWarnings(findPoliteTexts(text=phone_offers$message,
                                     type = "both",
                                     df_polite = df_polite,
                                     covar = phone_offers$condition,
                                     num_docs = 13))
  }, is_a("data.frame"))

  expect_that({
    suppressWarnings(findPoliteTexts(text = phone_offers$message,
                                     df_polite = df_polite,
                                     covar = phone_offers$condition,
                                     num_docs = 13))
  }, is_a("data.frame"))

  expect_that({
    suppressWarnings(findPoliteTexts(text = phone_offers$message,
                                     type = "least",
                                     df_polite = df_polite,
                                     covar = phone_offers$condition,
                                     num_docs = 5))
  }, is_a("data.frame"))



  expect_that({
    suppressWarnings(findPoliteTexts(text = phone_offers$message,
                                     df_polite = df_polite,
                                     covar = continous_condition,
                                     num_docs = 13))
  }, is_a("data.frame"))

  expect_that({
    suppressWarnings(findPoliteTexts(text = phone_offers$message,
                                     df_polite = df_polite,
                                     covar = continous_condition,
                                     type = "both",
                                     num_docs = 13))
  }, is_a("data.frame"))
})

