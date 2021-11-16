# Build package

# TODO
# 'spsurvey' gives errors (missing package) in remote checks -- work to remove from Suggests
#  deprecate functions
# 'moments' is used in plotHD -- work to remove it from Suggests

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

# Render README
rmarkdown::render("README.Rmd")

# check documentation ----
roxygen2::roxygenise()
devtools::check_man()
spelling::spell_check_package()
# spelling::update_wordlist()

# check examples ----
devtools::run_examples()

# check for Linux (local) ----
devtools::check(document = TRUE, manual = TRUE, force_suggests = TRUE, run_dont_test = TRUE)

# check for Windows (remote) ----
devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrelease()

# check in R-hub ----
# rhub::validate_email(email = 'alessandrosamuelrosa@gmail.com')
rhub::check_for_cran()
# rhub::check(platform = c('windows-x86_64-devel'))
# rhub::platforms()
# devtools::check_rhub(env_vars = c("_R_CHECK_FORCE_SUGGESTS_" = "false")) # scape missing suggested packages

# check all downstream dependencies
source("revdep/check.R")

# upload to CRAN ----
# devtools::build()
devtools::release(check = FALSE)
