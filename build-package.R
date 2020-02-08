# Build package

# update dependencies ----
update(remotes::package_deps(packages = "pedometrics"))
update(remotes::package_deps(packages = "devtools"))

# turn on/off development mode
# devtools::dev_mode()

# RCpp
# Avoid error
# Error in dyn.load(dllfile) : 
# unable to load shared object <...>.so
Rcpp::compileAttributes()

# check documentation ----
roxygen2::roxygenise()
devtools::check_man()
spelling::spell_check_package()
# spelling::update_wordlist()

# check examples ----
devtools::run_examples()

# check the package for Linux (local) ----
devtools::check(document = TRUE, manual = TRUE, force_suggests = TRUE, run_dont_test = TRUE)

# check the package for Windows (remote) ----
devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrelease()

# check the package in R-hub ----
# rhub::validate_email(email = 'alessandrosamuelrosa@gmail.com')
devtools::check_rhub()

# check all downstream dependencies
source("revdep/check.R")

# upload to CRAN ----
# devtools::build()
devtools::release(check = FALSE)
