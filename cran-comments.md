# CRAN comments

## Changes

Dear CRAN maintainers, this is a minor release of the package __pedometrics__ after it was removed from the CRAN repository on 2021-10-06 as check problems were not corrected in time. All suggested packages are now used conditionally on their availability: this was tested using the env var _R_CHECK_DEPENDS_ONLY_ = TRUE in R CMD check.

This minor release includes improvements to:

1. Deal with breaking changes in the formerly suggested package __spsurvey__.
1. Reduce the number of package dependencies: __pbapply__, __spsurvey__, and __xtable__.
1. Fix the following CRAN check notes:

Flavors: r-devel-linux-x86_64-fedora-clang, r-devel-linux-x86_64-fedora-gcc, r-patched-solaris-x86

```
 Version: 0.11.0
Check: package dependencies
Result: NOTE
    Packages suggested but not available for checking: 'geoR', 'georob'
```

Flavor: r-devel-linux-x86_64-fedora-clang

```
Version: 0.11.0
Check: Rd cross-references
Result: NOTE
    Undeclared packages ‘Hmisc’, ‘vcd’, ‘VecStatGraphs2D’, ‘mvtsplot’, ‘matrixStats’, ‘RandomFields’, ‘automap’, ‘spsann’ in Rd xrefs
```

Flavors: r-devel-linux-x86_64-fedora-clang, r-devel-linux-x86_64-fedora-gcc, r-patched-solaris-x86

```
Version: 0.11.0
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

This minor release should enable the __pedometrics__ package to return to CRAN.

## Test environments

* OK: local, x86_64-pc-linux-gnu (64-bit), Ubuntu 20.04.4 LTS, R version 4.1.3 (2022-03-10)
  * `devtools::check(env_vars = c(`_R_CHECK_DEPENDS_ONLY_` = TRUE, NOT_CRAN = TRUE))`
  * `devtools::check(document = TRUE, manual = TRUE, force_suggests = TRUE, run_dont_test = TRUE)`
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R version 4.2.0 RC (2022-04-19 r82220 ucrt)
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R version 4.1.3 (2022-03-10)
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R version 4.0.5 (2021-03-31)
* OK: rhub, Windows Server 2022, R-devel, 64 bit
* OK: rhub, Fedora Linux, R-devel, clang, gfortran
* OK: rhub, Ubuntu Linux 20.04.1 LTS, R-release, GCC
* OK: rhub, Debian Linux, R-devel, clang, ISO-8859-15 locale

## R CMD check results

There were no ERRORs or WARNINGs.

There was one NOTE in most test environments.

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>'

New submission

Package was archived on CRAN

Possibly mis-spelled words in DESCRIPTION:
  Pedometric (4:22)
  pedometrics (5:80)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2021-10-06 as check problems were not
    corrected in time.
```

This NOTE can be ignored.

There was one NOTE in most rhub test environments.

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```

This NOTE can be ignored.

## Downstream dependencies

There are no reverse dependencies.
