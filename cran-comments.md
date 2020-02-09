# Changes

This is a minor release. It includes changes to deal with the fact that matrix objects now also inherit from
class "array" and that the suggested package __geoR__ is an orphan package since 2020-01-12. Also, some 
functions are being deprecated because they are no longer (or rarely) used. These functions were developed to
fulfill the demands created by my PhD research project (2012-2016). Some functions will be replaced, but all
of them will be moved to a package currently under developed at https://github.com/samuel-rosa/ASRtools.

# Test environments

* OK: local, x86_64-pc-linux-gnu (64-bit), Ubuntu 18.04.3 LTS, R 3.6.2
* OK: travis-ci, x86_64-pc-linux-gnu (64-bit), Ubuntu 14.04.6 LTS, R 3.6.2
* OK: win-builder, x86_64-w64-mingw32 (64-bit), Windows, R 3.5.3
* OK: win-builder, x86_64-w64-mingw32 (64-bit), Windows, R 3.6.2
* OK: win-builder, x86_64-w64-mingw32 (64-bit), Windows, R Under development (unstable)
* OK: R-hub, Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* FAIL: R-hub, Ubuntu Linux 16.04 LTS, R-release, GCC
* FAIL: R-hub, Debian Linux, R-devel, GCC ASAN/UBSAN
* FAIL: R-hub, Fedora Linux, R-devel, clang, gfortran

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

```
* checking package dependencies ... ERROR
Package suggested but not available: ‘spsurvey’

Suggests orphaned package: ‘geoR’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

On Debian Linux:

```
4966#> Error: package or namespace load failed for ‘checkmate’ in dyn.load(file, DLLpath = DLLpath, ...):
4967#> unable to load shared object '/home/docker/R/checkmate/libs/checkmate.so':
4968#> /home/docker/R/checkmate/libs/checkmate.so: undefined symbol: LOGICAL_NO_NA
4969#> Error: loading failed
4970#> Execution halted
4971#> ERROR: loading failed
4972#> * removing ‘/home/docker/R/checkmate’
```

```
15160#> ERROR: dependency ‘htmlTable’ is not available for package ‘Hmisc’
15161#> * removing ‘/home/docker/R/Hmisc’
```

```
15193#> ERROR: dependency ‘Hmisc’ is not available for package ‘spsurvey’
15194#> * removing ‘/home/docker/R/spsurvey’
```

```
15223#> Warning messages:
15224#> 1: In i.p(...) :
15225#> installation of package ‘checkmate’ had non-zero exit status
15226#> 2: In i.p(...) :>
15227#> installation of package ‘htmlTable’ had non-zero exit status
15228#> 3: In i.p(...) : installation of package ‘Hmisc’ had non-zero exit status
15229#> 4: In i.p(...) :
15230#> installation of package ‘spsurvey’ had non-zero exit status
```

The __rhub__ package maintainer has been warned about this issue.

# R CMD check results

There were no ERRORs or WARNINGs.

There was one NOTE in most test environments.

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>'
```

This NOTE can be ignored.

# Downstream dependencies

I have also run R CMD check on the only downstream dependence
(https://github.com/samuel-rosa/pedometrics/tree/master/revdep), with no issues detected.
