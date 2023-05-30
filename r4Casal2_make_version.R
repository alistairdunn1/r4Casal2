# Get r4Casal2 Version (Needs manual editing)
source("Version.R")

# Build DESCRIPTION file
filename<-"r4Casal2/DESCRIPTION"
cat("Package: r4Casal2\nTitle: r4Casal2 R package for post proccessing Casal2 model runs\nVersion: ",file=filename)
cat(VersionNumber,file=filename,append=TRUE)
cat("\n",file=filename,append=TRUE)
cat("Author: C Marsh & A Dunn\n",file=filename,append=TRUE)
cat("Description: Accessor and utility functions to support the Casal2 R Library.\n",file=filename,append=TRUE)
cat("Maintainer: Casal2 Development Team <casal2@niwa.co.nz>  Alistair Dunn <alistair.dunn@oceanenvironmental.co.nz>\n",file=filename,append=TRUE)
cat("License: MIT + file LICENSE\n",file=filename,append=TRUE)
cat("LazyData: true\n",file=filename,append=TRUE)
cat("Imports: dplyr,\n    ggplot2,\n    reshape2,\n    DHARMa,\n    tidyr (>= 1.2.0),\n   knitr,\n    Rdpack,\n    bookdown,\n    stats,\n    randtests,\n    RColorBrewer\n",file=filename,append=TRUE)
cat("RoxygenNote: 7.2.3\n",file=filename,append=TRUE)
cat("Suggests: \n    Casal2\n",file=filename,append=TRUE)
cat("RdMacros: Rdpack\n",file=filename,append=TRUE)

# Create R function to return R library version number
filename<-"r4Casal2/R/Version.R"
cat("\"Version\" <-\n",file=filename)
cat("function() {\n",file=filename,append=T)
cat(paste("  return(\"",VersionNumber,"\")\n",sep=""),file=filename,append=T)
cat("}\n",file=filename,append=T)
