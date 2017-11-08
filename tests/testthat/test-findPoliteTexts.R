data("phone_offers")
polite.data<-politeness(phone_offers$message, parser="none",drop_blank=FALSE, num_mc_cores=1)

df_most <- suppressWarnings(findPoliteTexts(phone_offers$message,
                                            polite.data,
                                            phone_offers$condition,
                                            num_docs = 10))
df_both <- suppressWarnings(findPoliteTexts(phone_offers$message,
                                            type = "both",
                                            polite.data,
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

df_covar <- data.frame(condition = phone_offers$condition )
num_obs <- length(phone_offers$condition)
set.seed(100)
continous_condition <- ifelse(phone_offers$condition_logical,
                              stats::rnorm(num_obs,mean = 0),
                              stats::rnorm(num_obs, mean = 100))

test_that("find polite runs with different parameter" , {
  expect_that({
    suppressWarnings(findPoliteTexts(phone_offers$message,
                                     type = "both",
                                     polite.data,
                                     df_covar,
                                     num_docs = 13))
  }, is_a("data.frame"))

  expect_that({
    suppressWarnings(findPoliteTexts(phone_offers$message,
                                     polite.data,
                                     df_covar,classifier="mnir",
                                     num_docs = 13))
  }, is_a("data.frame"))

  expect_that({
    suppressWarnings(findPoliteTexts(phone_offers$message,
                                     type = "both",
                                     polite.data,
                                     df_covar$condition,classifier="mnir",
                                     num_docs = 13))
  }, is_a("data.frame"))


  expect_that({
    suppressWarnings(findPoliteTexts(phone_offers$message,
                                     polite.data,
                                     stats::rnorm(n=num_obs),classifier="mnir",
                                     num_docs = 13))
  }, is_a("data.frame"))

  expect_that({
    suppressWarnings(findPoliteTexts(phone_offers$message,
                                     polite.data,
                                     stats::rnorm(n=num_obs),
                                     num_docs = 13))
  }, is_a("data.frame"))
})
