# df_polite_orig <- df_polite
#
# v_i = 1:10
# df_polite <- df_polite_orig[v_i, 1, drop=FALSE]
# split = phone_offers[["condition_logical_rev"]][v_i]
# split_levels = c("polite","not polite")
# split_name =NULL
# split_cols=c("firebrick","navy")
# top_title = ""
# drop_blank=0.05




# upload data

data("phone_offers")

# make politness data frames
df_polite <- politeness(text = phone_offers$message, parser = "none", binary = TRUE, num_mc_cores=1)

phone_offers$condition_factor <- factor(phone_offers$condition)
levels(phone_offers$condition_factor) <- c("bbbb","aaaa")

phone_offers$condition_character <- as.character(phone_offers$condition_factor)
phone_offers$condition_integer <- as.integer(phone_offers$condition)
phone_offers$condition_logical <- as.logical(phone_offers$condition)
phone_offers$condition_logical_rev <- !phone_offers$condition_logical
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
                   split = phone_offers[["condition_logical_rev"]],
                   split_levels = c("polite","not polite"),
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

  expect_that({
    politenessPlot(df_polite,
                   split = -1 * phone_offers[["condition_integer"]],
                   split_levels = c("polite","not polite"),
                   split_name =NULL,
                   top_title = ""
    )
  }, is_a("ggplot"))

})







