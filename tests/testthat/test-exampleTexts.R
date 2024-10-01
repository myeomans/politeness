data("phone_offers")
df_polite <-politeness(phone_offers$message, parser="none",drop_blank=FALSE, num_mc_cores=1)

df_most <- suppressWarnings(exampleTexts(phone_offers$message,
                                            phone_offers$condition,
                                            num_docs = 10))

test_that("find polite correct dimensions",{
  expect_equal(length(df_most) , 10)
})

test_that("find polite in original data",{
  expect_true(all(df_most %in% phone_offers$message))
})

num_obs <- length(phone_offers$condition)
set.seed(100)
continous_condition <- ifelse(as.logical(phone_offers$condition),
                              stats::rnorm(num_obs,mean = 5),
                              stats::rnorm(num_obs, mean = 1))

test_that("find polite runs with different parameter" , {

  expect_that({
    suppressWarnings(exampleTexts(text = phone_offers$message,
                                     covar = phone_offers$condition,
                                     num_docs = 13))
  }, is_a("character"))

  expect_that({
    suppressWarnings(exampleTexts(text = phone_offers$message,
                                     type = "least",
                                     covar = phone_offers$condition,
                                     num_docs = 5))
  }, is_a("character"))



  expect_that({
    suppressWarnings(exampleTexts(text = phone_offers$message,
                                     covar = continous_condition,
                                     num_docs = 13))
  }, is_a("character"))

})

