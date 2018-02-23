# Build package

# Dependencies
update(devtools::package_deps())

# turn on/off development mode
devtools::dev_mode()

# RCpp
# Avoid error
# Error in dyn.load(dllfile) : 
# unable to load shared object <...>.so
Rcpp::compileAttributes()

# check examples and documentation
roxygen2::roxygenise()
devtools::check_man()
devtools::run_examples(run = TRUE)

# check the package for Linux and Windows
devtools::check(
  document = TRUE, manual = TRUE, check_version = TRUE, force_suggests = TRUE, args = "--use-valgrind")
devtools::build_win()
devtools::build()

# Check all downstream dependencies
source("revdep/check.R")

# Upload to CRAN
devtools::release(check = FALSE)


