# if your R path is == 'C:/Projects/Software/r4Casal2/r4Casal2/inst/testdata/ComplexTag/Bookdown'. Then this script should run
# We will check anyway
if(getwd() != 'C:/Projects/Software/r4Casal2/r4Casal2/inst/testdata/ComplexTag/Bookdown') {
 print('You need to change your working directory to C:/Projects/Software/r4Casal2/r4Casal2/inst/testdata/ComplexTag/Bookdown. to recompile the bookdown.')
  setwd('C:/Projects/Software/r4Casal2/r4Casal2/inst/testdata/ComplexTag/Bookdown')
}
## Now Recompile
library(bookdown)
render_book()
