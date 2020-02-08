# Changes

This is a patch.

# Test environments

* OK: local, x86_64-pc-linux-gnu (64-bit), Ubuntu 18.04.3 LTS, R 3.6.2

* OK: travis-ci, x86_64-pc-linux-gnu (64-bit), Ubuntu 14.04.6 LTS, R 3.6.2
* OK: win-builder, x86_64-w64-mingw32 (64-bit), Windows, R 3.5.3
* OK: win-builder, x86_64-w64-mingw32 (64-bit), Windows, R 3.6.2
* OK: R-hub, Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* FAIL: R-hub, Ubuntu Linux 16.04 LTS, R-release, GCC
* FAIL: R-hub, Debian Linux, R-release, GCC
* FAIL: R-hub, Fedora Linux, R-devel, clang, gfortran

* OK: win-builder, x86_64-w64-mingw32 (64-bit), Windows, R Under development (unstable)

Failure in R-hub test environments are due to missing software and package dependencies in those test
environments.

On Ubuntu Linux:

```
* checking package dependencies ... ERROR
Package suggested but not available: ‘spsurvey’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

On Fedora Linux:


On Debian Linux:



The __rhub__ package maintainer has been warned about this issue.

# R CMD check results

There were no ERRORs or WARNINGs.

There were three NOTEs in R-hub test environments.

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>’

* checking Rd cross-references ... NOTE
Packages unavailable to check Rd xrefs: 'vcd', 'VecStatGraphs2D', 'mvtsplot', 'matrixStats', 'automap', 'spsann'

* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  'examples_i386' 'examples_x64' 'pedometrics-Ex_i386.Rout'
  'pedometrics-Ex_x64.Rout'
```

These NOTEs can be ignored.

There was one NOTE in winbuilder test environments.

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>'
```

This NOTE can be ignored.

# Downstream dependencies

I have also run R CMD check on the only downstream dependence
(https://github.com/samuel-rosa/pedometrics/tree/master/revdep). I have noted
the following unimportant NOTEs:

```
checking Rd cross-references ... NOTE
Packages unavailable to check Rd xrefs: ‘spcosa’, ‘clhs’, ‘raster’, ‘geoR’
```
```
DONE
Status: 1 NOTE
```
