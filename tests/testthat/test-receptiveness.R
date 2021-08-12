#
# Note - doesn't run on CRAN check server
#
# data("phone_offers")
# phone_offers <- phone_offers[1:10,]
#
# context("receptiveness function")
#
# test_that("empty or na string", {
#  phone_offers$message[1] <- ""
#
#  v_recept <- receptiveness(c("","a"))
#  expect_equal(length(v_recept),2)
#
#  v_recept <- receptiveness(c(NA_character_,"a"))
#  expect_equal(length(v_recept),2)
#
#  phone_offers$message[1] <- NA_character_
#
#  v_recept <- receptiveness(phone_offers$message)
#  expect_equal(length(v_recept),length(phone_offers$message))
# })
#
#
# test_that("factor", {
#   phone_offers$message[1] <- ""
#   phone_offers$message <- factor(phone_offers$message)
#   v_recept <- receptiveness(phone_offers$message)
#   expect_equal(length(v_recept),length(phone_offers$message))
# })
#
# test_that("text of length 0", {
#   v_recept <- receptiveness(c(phone_offers$message[2],""))
#   expect_equal(length(v_recept),2)
# })
#
#
# test_that("text of length 1", {
#   v_recept <- receptiveness("hello")
#   expect_equal(length(v_recept),1)
# })
#
#
#
#

