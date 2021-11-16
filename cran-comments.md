# CRAN comments

## Changes

Dear CRAN maintainers, this is a minor release of the package __pedometrics__.

This minor release includes improvements to (a) deal with possibly breaking changes in a suggested
package (`spsurvey`), (b) reduce the number of package dependencies (__pbapply__ was removed from
Suggests), and (c) fix the following CRAN check note:

```
Version: 0.7.0
Check: Rd cross-references
Result: NOTE
    Undeclared packages ‘Hmisc’, ‘vcd’, ‘VecStatGraphs2D’, ‘mvtsplot’, ‘matrixStats’,
    ‘RandomFields’, ‘automap’, ‘spsann’ in Rd xrefs
Flavor: r-devel-linux-x86_64-fedora-clang
```

## Test environments

This minor release of the package __pedometrics__ was tested in the following environments:

* OK: local, x86_64-pc-linux-gnu (64-bit), Ubuntu 20.04.2 LTS, 4.1.0 (2021-05-18)
* OK: R-hub, Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* OK: win-builder, x86_64-w64-mingw32 (64-bit), Windows, R Under development (unstable) (2021-08-13 r80752)
* PREPERROR: R-hub, Debian Linux, R-devel, GCC ASAN/UBSAN


* OK: win-builder, x86_64-w64-mingw32 (64-bit), Windows, R 3.5.3
* OK: win-builder, x86_64-w64-mingw32 (64-bit), Windows, R 3.6.2
* FAIL: R-hub, Ubuntu Linux 16.04 LTS, R-release, GCC
* FAIL: R-hub, Fedora Linux, R-devel, clang, gfortran


Failure in R-hub test environments are due to missing software and package dependencies in those
test environments.

On Ubuntu Linux:

On Fedora Linux:

On Debian Linux, configuration failed for packages ‘units’, ‘rgeos’, ‘s2’, and ‘SpatialTools’.

The maintainer of R-hub are aware of this issue. Next versions of package __pedometrics__ will
possibly be less dependent on such packages.

## R CMD check results

There were no ERRORs or WARNINGs.

There was one NOTE in most test environments.

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>'
```

This NOTE can be ignored.

## Downstream dependencies

I have also run R CMD check on the only downstream dependence
(https://github.com/samuel-rosa/pedometrics/tree/master/revdep), with no issues detected.
