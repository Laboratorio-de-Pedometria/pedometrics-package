# Build package

# Test package #####################################################################################
# install.packages("rgeos", dependencies = TRUE)
# install.packages("Matrix", dependencies = TRUE)
# Dependencies
update(remotes::package_deps(packages = "pedometrics"))
update(remotes::package_deps(packages = "devtools"))
# Reverse dependency tools
devtools::revdep("pedometrics-package/")
# Render README
rmarkdown::render("pedometrics-package/README.Rmd")

# RCpp
# Avoid error
# Error in dyn.load(dllfile) :
# unable to load shared object <...>.so
Rcpp::compileAttributes("pedometrics-package/")

# check documentation ----
roxygen2::roxygenise("pedometrics-package/")
devtools::check_man("pedometrics-package/")
spelling::spell_check_package("pedometrics-package/")
# spelling::update_wordlist()

# check examples ----
devtools::run_examples("pedometrics-package/")

# check for Linux (local) ----
devtools::check("pedometrics-package/",
  env_vars = c(`_R_CHECK_DEPENDS_ONLY_` = TRUE))
devtools::check("pedometrics-package/",
  document = TRUE, manual = TRUE, vignettes = TRUE, force_suggests = TRUE, incoming = TRUE,
  remote = TRUE)

# check for Windows (remote) ----
devtools::check_win_devel("pedometrics-package/")
devtools::check_win_release("pedometrics-package/")
devtools::check_win_oldrelease("pedometrics-package/")

# check in R-hub ----
# rhub::validate_email(email = "alessandrosamuelrosa@gmail.com")
# rhub::check_on_windows()
# rhub::platforms()
platforms <- c("fedora-clang-devel",
  "ubuntu-gcc-release", "debian-clang-devel", "windows-x86_64-devel")
devtools::check_rhub( "pedometrics-package/",
  platforms = platforms, interactive = FALSE, env_vars = c(`_R_CHECK_FORCE_SUGGESTS_` = "false"))

# upload to CRAN ----
# devtools::build()
devtools::release(check = FALSE)


devtools::load_all()
