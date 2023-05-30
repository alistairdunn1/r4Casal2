# build package
# this builds all the help files
library(devtools)
library(roxygen2)

source("r4Casal2_make_version.R")

document("r4Casal2")
build("r4Casal2")
# devtools::install()
# devtools::check() # doesn't like how we have put Casal2 in the Suggests:
testthat::test_dir("r4Casal2/tests/testthat/")
## build bookdown locally
bookdown::render_book(input = "r4Casal2/GitBook/")

# Final build for clean version for distribution
devtools::build("r4Casal2", binary = TRUE, args = c("--preclean"))
