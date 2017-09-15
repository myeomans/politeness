# upload data

data("phone_offers")

# make politness data frames
df_polite <- politeness(text = phone_offers$message, parser = "none", binary = TRUE)

phone_offers$condition_factor <- factor(phone_offers$condition)
levels(phone_offers$condition_factor) <- c("Cold","Warm")

phone_offers$condition_character <- as.character(phone_offers$condition_factor)
phone_offers$condition_integer <- as.integer(phone_offers$condition)
phone_offers$condition_logical <- as.logical(phone_offers$condition)
# test logical

test_that("plot with different split variable" , {
  expect_that({
    politenessPlot(df_polite,
                   split = phone_offers[["condition_logical"]],
                   split_levels = c("not polite","polite"),
                   split_name =NULL,
                   top_title = ""
    )
  }, is_a("ggplot"))

  expect_that({
    politenessPlot(df_polite,
                   split = phone_offers[["condition_factor"]],
                   split_levels = NULL,
                   split_name =NULL,
                   top_title = ""
    )
  }, is_a("ggplot"))

  expect_that({
    politenessPlot(df_polite,
                   split = phone_offers[["condition_character"]],
                   split_levels = c("not polite","polite"),
                   split_name =NULL,
                   top_title = "title"
    )
  }, is_a("ggplot"))


  expect_that({
    politenessPlot(df_polite,
                   split = phone_offers[["condition_integer"]],
                   split_levels = c("not polite","polite"),
                   split_name =NULL,
                   top_title = ""
    )
  }, is_a("ggplot"))

})







