#'
#' @details The purpose is to check the build_assessment_bookdown function works
#' across a suite of models
#'

test_that("build_assessment_bookdown", {
  test_data_dir = normalizePath(file.path("..","..","inst","testdata"), winslash = "/")
  #test_data_dir = normalizePath(file.path("inst","testdata"), winslash = "/")

  ## ComplexTag
  expect_message(build_assessment_bookdown(csl_dir = file.path(test_data_dir, "ComplexTag"), output_folder_name = "Bookdown", mpd_filename = "estimate_betadiff.log", config_filename = "config_betadiff.csl2", model_label = "ComplexTag", verbose = T, prompt_user_before_deleting = F))

  ## SBW
  expect_message(build_assessment_bookdown(csl_dir = file.path(test_data_dir, "SBW"), output_folder_name = "Bookdown", mpd_filename = "estimate_betadiff.log", config_filename = "config_betadiff.csl2", model_label = "SBW", verbose = T, prompt_user_before_deleting = F))

  ## TwoSex
  expect_message(build_assessment_bookdown(csl_dir = file.path(test_data_dir, "TwoSex"), output_folder_name = "Bookdown", mpd_filename = "estimate_betadiff.log", config_filename = "config_betadiff.csl2", model_label = "TwoSex", verbose = T, prompt_user_before_deleting = F))

  ## SexedLengthBased
  expect_message(build_assessment_bookdown(csl_dir = file.path(test_data_dir, "SexedLengthBased"), output_folder_name = "Bookdown", mpd_filename = "estimate_betadiff.log", config_filename = "config_betadiff.csl2", model_label = "SexedLengthBased", verbose = T, prompt_user_before_deleting = F))

})
