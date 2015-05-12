################################################################################
# Build packages
################################################################################

# Load packages
library(devtools)

# Build Rcpp files
library(Rcpp)
setwd("~/PROJECTS/r-packages/pedometrics") ## laptop
Rcpp::compileAttributes()

# Generate documentation using Rd2oxygen2 ######################################
require(Rd2roxygen)
setwd("~/PROJECTS/r-packages/pedometrics")
roxygen2::roxygenise()

# Built and check package
setwd("~/alessandro") # ISRIC desktop and LGCS-MDS
system("R CMD build pedometrics")
system("R CMD check --as-cran pedometrics_0.5-1.tar.gz")

setwd("~/alessandro/pedometrics") # ISRIC desktop and LGCS-MDS

