# build package
# this builds all the help files
library(devtools)
library(roxygen2)
document();
build()
# devtools::install()
# devtools::check() # doesn't like how we have put Casal2 in the Suggests:
testthat::test_dir("tests/testthat/")
## build bookdown locally
bookdown::render_book(input = "GitBook/")

