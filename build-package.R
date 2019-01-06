# Build package

# Dependencies
update(remotes::package_deps(packages = "pedometrics"))
update(remotes::package_deps(packages = "devtools"))

# turn on/off development mode
devtools::dev_mode()

# Rcpp::compileAttributes()

# check examples and documentation
devtools::check_man()
devtools::run_examples(run = FALSE)
devtools::spell_check()
devtools::check_rhub()

# check the package for Linux and Windows
devtools::check(document = TRUE, manual = TRUE, force_suggests = TRUE, run_dont_test = TRUE)

devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrelease()

devtools::build()

# Check all downstream dependencies
source("revdep/check.R")

# Upload to CRAN
devtools::release(check = FALSE)


