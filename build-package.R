# Build package

# Dependencies
update(remotes::package_deps("pedometrics"))
update(remotes::package_deps(packages = "devtools"))

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
devtools::run_examples()
devtools::spell_check()
devtools::check_rhub()

# check the package for Linux (local)
devtools::check(document = TRUE, manual = TRUE, force_suggests = TRUE, run_dont_test = TRUE)

# check the package for Windows (remote)
devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrelease()

# Check all downstream dependencies
source("revdep/check.R")

devtools::build()

# Upload to CRAN
devtools::release(check = FALSE)


