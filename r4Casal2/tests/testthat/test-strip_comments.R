#'
#' @details The purpose is to test the StripComments function works
#' across a suite of models
#'

test_that("StripComments", {
  ## remove first line
  temp_file = c(
"#this is a comment",
"This will remain")
  expect_equal(StripComments(temp_file), "This will remain")

  ## comment at end of line
  temp_file = "This will remain#this is a comment"
  expect_equal(StripComments(temp_file), "This will remain")

  ## multiline comments
  temp_file = c(
    "/*this is a comment",
    "and this is a comment",
    "and this is a comment",
    "*/",
    "This will remain")
  expect_equal(StripComments(temp_file), "This will remain")

  ## multiline comment that ends at the end of another line
  temp_file = c(
    "/*this is a comment",
    "and this is a comment",
    "and this is a comment*/",
    "This will remain")
  expect_equal(StripComments(temp_file), "This will remain")

  ## two multiline comments
  temp_file = c(
    "/*this is a comment*/",
    "/*this is a comment",
    "and this is a comment",
    "and this is a comment*/",
    "This will remain")
  expect_equal(StripComments(temp_file), "This will remain")

  ## two multiline comments
  temp_file = c(
    "/*this is a comment*/",
    "/*this is a comment*/",
    "This will remain")
  expect_equal(StripComments(temp_file), "This will remain")

})
