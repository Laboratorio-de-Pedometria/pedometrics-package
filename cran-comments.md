# CRAN comments

## Changes

Dear CRAN maintainers, this is a minor release of the package __pedometrics__.
It is th first release after __pedometrics__ was removed from the CRAN repository on 2021-10-06.
The package was removed from CRAN because some check problems were not corrected in time.
The key problem was the use of suggested packages unconditionally on their availability.
This problem was corrected and tested using the env var _R_CHECK_DEPENDS_ONLY_ = TRUE in R CMD check.

One of the suggested packages, __georob__, was removed from the CRAN repository on 2022-05-04.
It was removed from CRAN because it requires the also archived package __RandomFields__.
The latter was archived on 2022-05-04 as check problems were not corrected in time.
The following actions were taken to deal with this issue:

* __georob__ is used conditionally on their availability.
* All function help files point out if any suggested package is required for the function to work.
* The DESCRIPTION file explains where users can find former versions of any suggested packages.

Overall, this minor release includes the following improvements:

1. Use suggested packages conditionally on their availability.
1. Deal with breaking changes in the formerly suggested package __spsurvey__.
1. Deal with Suggests not in mainstream repositories: __georob__.
1. Reduce the number of package dependencies: __geoR__, __moments__, __plyr__, __pbapply__,
   __spsurvey__, and __xtable__.
1. Extended documentation.
1. Fix the following CRAN check notes:

Flavors: r-devel-linux-x86_64-fedora-clang, r-devel-linux-x86_64-fedora-gcc, r-patched-solaris-x86

```
Version: 0.7.0
Check: package dependencies
Result: NOTE
    Packages suggested but not available for checking: 'geoR', 'georob'
```

Flavor: r-devel-linux-x86_64-fedora-clang

```
Version: 0.7.0
Check: Rd cross-references
Result: NOTE
    Undeclared packages ‘Hmisc’, ‘vcd’, ‘VecStatGraphs2D’, ‘mvtsplot’, ‘matrixStats’, ‘RandomFields’, ‘automap’, ‘spsann’ in Rd xrefs 
```

Flavors: r-devel-linux-x86_64-fedora-clang, r-devel-linux-x86_64-fedora-gcc, r-patched-solaris-x86

```
Version: 0.7.0
Check: examples
Result: ERROR
    Running examples in ‘pedometrics-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: vgmICP
    > ### Title: Initial covariance parameters (ICP)
    > ### Aliases: vgmICP
    >
    > ### ** Examples
    >
    > data(meuse, package = "sp")
    > icp <- vgmICP(z = log(meuse$copper), coords = meuse[, 1:2])
    Error: Package(s) needed for this function to work but not installed: georob
    Execution halted
```

## Test environments

* OK: local, x86_64-pc-linux-gnu (64-bit), Ubuntu 20.04.4 LTS, R version 4.1.3 (2022-03-10)
  * `devtools::check(env_vars = c(`_R_CHECK_DEPENDS_ONLY_` = TRUE, NOT_CRAN = TRUE))`
  * `devtools::check(document = TRUE, manual = TRUE, force_suggests = TRUE, run_dont_test = TRUE)`
* OK: rhub, Windows Server 2022, R-devel, 64 bit
* OK: rhub, Fedora Linux, R-devel, clang, gfortran
* OK: rhub, Ubuntu Linux 20.04.1 LTS, R-release, GCC
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R Under development (unstable) (2022-05-27 r82411 ucrt)
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R version 4.2.0 (2022-04-22 ucrt)
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R version 4.1.3 (2022-03-10)
* OK: rhub, Debian Linux, R-devel, clang, ISO-8859-15 locale

## R CMD check results

There were no ERRORs or WARNINGs.

There was one NOTE in most test environments.

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>’
  
  New submission
  
  Package was archived on CRAN
  
  Possibly misspelled words in DESCRIPTION:
    Pedometric (4:22)
    geostatistical (10:5)
    pedometrics (5:70)
    spatio (6:65)
  
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
