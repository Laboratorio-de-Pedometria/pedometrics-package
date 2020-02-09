# library("devtools")

# res <- devtools::revdep_check()
# res <- devtools::revdep()
# devtools::revdep_check_save_summary(res)

# https://github.com/r-lib/revdepcheck
library(revdepcheck)

# Check package in working directory
# Will automatically create revdep/ directory if it doesn't already exist
revdep_check(num_workers = 4)

# Clear out all previous results
revdep_reset()
