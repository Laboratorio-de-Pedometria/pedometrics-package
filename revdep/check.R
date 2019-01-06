library("devtools")

res <- devtools::revdep_check()
devtools::revdep_check_save_summary(res)
