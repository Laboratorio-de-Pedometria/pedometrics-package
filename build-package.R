# Build package

# TODO
# 'moments' is used in plotHD -- work to remove it from Suggests

# Test package #####################################################################################
# install.packages("rgeos", dependencies = TRUE)
# install.packages("Matrix", dependencies = TRUE)
# Dependencies
update(remotes::package_deps(packages = "pedometrics"))
update(remotes::package_deps(packages = "devtools"))
# Reverse dependency tools
devtools::revdep()
# Render README
rmarkdown::render("README.Rmd")

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

# check for Linux (local) ----
devtools::check(env_vars = c(`_R_CHECK_DEPENDS_ONLY_` = TRUE))
devtools::check(
  document = TRUE, manual = TRUE, vignettes = TRUE, force_suggests = TRUE, incoming = TRUE,
  remote = TRUE)

# check for Windows (remote) ----
devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrelease()

# check in R-hub ----
# rhub::validate_email(email = "alessandrosamuelrosa@gmail.com")
# rhub::check_on_windows()
# rhub::platforms()
platforms <- c("fedora-clang-devel",
  "ubuntu-gcc-release", "debian-clang-devel", "windows-x86_64-devel")
devtools::check_rhub(platforms = platforms)

# upload to CRAN ----
# devtools::build()
devtools::release(check = FALSE)
