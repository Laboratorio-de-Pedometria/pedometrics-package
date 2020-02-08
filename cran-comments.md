# Changes

This is a patch. It includes changes to deal with the fact that matrix objects now also inherit from class 
"array" and that the suggested package __geoR__ is an orphan package since 2020-01-12.

# Test environments

* OK: local, x86_64-pc-linux-gnu (64-bit), Ubuntu 18.04.3 LTS, R 3.6.2
* OK: travis-ci, x86_64-pc-linux-gnu (64-bit), Ubuntu 14.04.6 LTS, R 3.6.2
* OK: win-builder, x86_64-w64-mingw32 (64-bit), Windows, R 3.5.3
* OK: win-builder, x86_64-w64-mingw32 (64-bit), Windows, R 3.6.2
* OK: win-builder, x86_64-w64-mingw32 (64-bit), Windows, R Under development (unstable)
* FAIL: R-hub, Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* FAIL: R-hub, Ubuntu Linux 16.04 LTS, R-release, GCC
* FAIL: R-hub, Debian Linux, R-devel, GCC ASAN/UBSAN
* FAIL: R-hub, Fedora Linux, R-devel, clang, gfortran

Failure in R-hub test environments are due to missing software and package dependencies in those test
environments.

On Windows:

```
* checking package dependencies ... ERROR
  'sp', 'SpatialTools', 'spsurvey', 'xtable'

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section 'The DESCRIPTION file' in the 'Writing R Extensions'
manual.
```

Apparently, this ERROR occurs due to no space left on device:

```
943#> package 'readr' successfully unpacked and MD5 sums checked
944#> cannot open file 'C:/Users/USERaWvZBPGSxk/R/filee30f2248aa/rematch/NAMESPACE': No space left on device
945#> Error in unzip(zipname, exdir = dest) :
946#> Calls: ... .install.winbinary -> unpackPkgZip -> .zip.unpack -> unzip
947#> In unzip(zipname, exdir = dest) : write error in extracting from zip file
948#> In addition: Warning message:
949#> Execution halted
950#> setting _R_CHECK_FORCE_SUGGESTS_ to false
```

The __rhub__ package maintainer [has been warned](https://github.com/r-hub/rhub/issues/176) about this issue.

On Ubuntu Linux and Fedora Linux:

```
* checking package dependencies ... ERROR
Package suggested but not available: ‘spsurvey’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

This ERROR occurs due to a missing dependency of __spsurvey__:

```
6322#> ERROR: dependency ‘sf’ is not available for package ‘spsurvey’
```

The __rhub__ package maintainer [has been warned](https://github.com/r-hub/rhub/issues/341) about this issue.

On Fedora Linux there also was an ERROR due to the suggesting an orphaned package, i.e. __geoR__.

On Debian Linux:

```
4995#> Error: package or namespace load failed for ‘checkmate’ in dyn.load(file, DLLpath = DLLpath, ...):
4996#> unable to load shared object '/home/docker/R/checkmate/libs/checkmate.so':
4997#> /home/docker/R/checkmate/libs/checkmate.so: undefined symbol: LOGICAL_NO_NA
4998#> Error: loading failed
4999#> Execution halted
5000#> ERROR: loading failed
5001#> * removing ‘/home/docker/R/checkmate’
```

```
14510#> ERROR: dependency ‘checkmate’ is not available for package ‘htmlTable’
14511#> * removing ‘/home/docker/R/htmlTable’
```

```
15222#> ERROR: dependency ‘Hmisc’ is not available for package ‘spsurvey’
15223#> * removing ‘/home/docker/R/spsurvey’
```

On Debian Linux there also were WARNINGs:

```
6005#> Warning: S3 methods ‘ggplot2::autoplot.zoo’, ‘ggplot2::fortify.zoo’, ‘ggplot2::scale_type.yearmon’, ‘ggplot2::scale_type.yearqtr’ were declared in NAMESPACE but not found
```

```
6034#> Warning: S3 methods ‘spatstat::as.im.RasterLayer’, ‘spatstat::as.im.SpatialGridDataFrame’, ‘spatstat::as.linnet.SpatialLines’, ‘spatstat::as.owin.SpatialGridDataFrame’, ‘spatstat::as.owin.SpatialPixelsDataFrame’, ‘spatstat::as.owin.SpatialPolygons’, ‘spatstat::as.ppp.SpatialPoints’, ‘spatstat::as.ppp.SpatialPointsDataFrame’, ‘spatstat::as.psp.Line’, ‘spatstat::as.psp.Lines’, ‘spatstat::as.psp.SpatialLines’, ‘spatstat::as.psp.SpatialLinesDataFrame’ were declared in NAMESPACE but not found
```

```
14434#> Warning: S3 methods ‘timeSeries::as.timeSeries.xts’, ‘fts::as.fts.xts’ were declared in NAMESPACE but not found
```

```
15196#> Warning: S3 method ‘xts::as.xts.data.table’ was declared in NAMESPACE but not found
```
The __rhub__ package maintainer [has been warned](https://github.com/r-hub/rhub/issues/343) about this issue.

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
(https://github.com/samuel-rosa/pedometrics/tree/master/revdep). I have noted
the following unimportant NOTE:

```
checking Rd cross-references ... NOTE
Packages unavailable to check Rd xrefs: ‘spcosa’, ‘clhs’, ‘raster’, ‘geoR’
```
