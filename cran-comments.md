# CRAN comments

## Changes

Dear CRAN maintainers, this is a patch release of the package __pedometrics__.
It fixes one of the notes issue on the last CRAN check results
(https://cran.r-project.org/web/checks/check_results_pedometrics.html).
The note was issued because escaped LaTeX specials were found in the documentation of `plotESDA()`.
These escaped LaTeX specials were removed in this patch release.

One of the suggested packages, __georob__, was removed from the CRAN repository on 2022-05-04.
It was removed from CRAN because it requires the also archived package __RandomFields__.
The latter was archived on 2022-05-04 as check problems were not corrected in time.
The following actions were taken to deal with this issue:

* __georob__ is used conditionally on their availability.
* All function help files point out if any suggested package is required for the function to work.
* The DESCRIPTION file explains where users can find former versions of any suggested packages.

## Test environments

* OK: local, x86_64-pc-linux-gnu (64-bit), Ubuntu 20.04.4 LTS, R version 4.2.0 (2022-04-22)
  * `devtools::check(env_vars = c(`_R_CHECK_DEPENDS_ONLY_` = TRUE, NOT_CRAN = TRUE))`
  * `devtools::check(document = TRUE, manual = TRUE, force_suggests = TRUE, run_dont_test = TRUE)`
* OK: rhub, Windows Server 2022, R-devel, 64 bit
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R version 4.2.0 (2022-04-22 ucrt)
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R Under development (unstable) (2022-06-17 r82501 ucrt)
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R version 4.1.3 (2022-03-10)
* OK: rhub, Ubuntu Linux 20.04.1 LTS, R-release, GCC
* OK: rhub, Fedora Linux, R-devel, clang, gfortran
* OK: rhub, Debian Linux, R-devel, clang, ISO-8859-15 locale

## R CMD check results

There were no ERRORs or WARNINGs.

There was one NOTE in most test environments.

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>'

Suggests or Enhances not in mainstream repositories:
  georob
```

This NOTE can be ignored.

There were two NOTEs in most rhub test environments.

```
* checking package dependencies ... NOTE
Package suggested but not available for checking: 'georob'

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```

The first NOTE is due to the temporary removal of __georob__ and __RandomFields__ from the CRAN
repository. The second NOTE can be ignored.

## Downstream dependencies

There are no reverse dependencies.
