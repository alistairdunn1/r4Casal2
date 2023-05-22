#'
#' @details The purpose is to test the summarise_config function works
#' across a suite of models
#'

#' A non spatial GLM model with  Gamma log link response variable
test_that("summarise_config", {
  test_data_dir = file.path("..","..","inst","testdata")
  ## Complex
  summarise_config = tryCatch(expr = summarise_config(config_dir = file.path(test_data_dir, "Complex"), config_file = "config.csl2"), error = function(e){e})
  expect_false(inherits(summarise_config, "error"))

  ## ComplexTag
  summarise_config = tryCatch(expr = summarise_config(config_dir = file.path(test_data_dir, "ComplexTag"), config_file = "config_betadiff.csl2"), error = function(e){e})
  expect_false(inherits(summarise_config, "error"))

  ## SBW
  summarise_config = tryCatch(expr = summarise_config(config_dir = file.path(test_data_dir, "SBW"), config_file = "config_betadiff.csl2"), error = function(e){e})
  expect_false(inherits(summarise_config, "error"))

  ## SexedLengthBased
  summarise_config = tryCatch(expr = summarise_config(config_dir = file.path(test_data_dir, "SexedLengthBased"), config_file = "config_betadiff.csl2"), error = function(e){e})
  expect_false(inherits(summarise_config, "error"))

  ## Simple
  summarise_config = tryCatch(expr = summarise_config(config_dir = file.path(test_data_dir, "Simple"), config_file = "config_betadiff.csl2"), error = function(e){e})
  expect_false(inherits(summarise_config, "error"))

  ## SingleSexTagByLength_input
  summarise_config = tryCatch(expr = summarise_config(config_dir = file.path(test_data_dir, "SingleSexTagByLength_input"), config_file = "config.csl2"), error = function(e){e})
  expect_false(inherits(summarise_config, "error"))

  ## TwoSex
  summarise_config = tryCatch(expr = summarise_config(config_dir = file.path(test_data_dir, "TwoSex"), config_file = "config_betadiff.csl2"), error = function(e){e})
  expect_false(inherits(summarise_config, "error"))
})
