## Changes
* Improved existing functions
* Created new functions
* Fix a test failure on R-devel

## Test environments
* ubuntu 12.04 (on travis-ci), R 3.2.1
* x86_64-pc-linux-gnu (ubuntu 14.04), R version 3.2.1 (2015-06-18)
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTEs (win-builder):

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>'

Possibly mis-spelled words in DESCRIPTION:
  Alessandro (35:57)
  Anjos (36:42)
  BEX (40:38)
  DF (40:5)
  Educa (39:58)
  Embrapa (37:52)
  HC (36:39)
  Heuvelink (38:18)
  ISRIC (38:29)
  Janeiro (37:12)
  Minist (39:44)
  Pedometric (2:8)
  Universidade (36:49)
  Vasques (37:43)
  da (39:55)
  de (37:9)
  functionalities (41:46)
  lia (39:73)
  pedometric (42:39)
  pedometrics (31:37, 32:64)
  rio (39:51)
```
```
* checking package dependencies ... NOTE
  No repository set, so cyclic dependency check skipped
```

## Downstream dependencies
I have also run R CMD check on the only downstream dependence of 
***pedometrics*** (https://github.com/samuel-rosa/pedometrics/tree/master/revdep).
I have noted the ERROR and NOTEs below.

```
checking dependencies in R code ... NOTE
Missing or unexported objects:
  ‘pedometrics::is.all.factor’ ‘pedometrics::is.any.factor’
  ‘pedometrics::is.numint’
```
```
checking Rd cross-references ... NOTE
Packages unavailable to check Rd xrefs: ‘spcosa’, ‘clhs’, ‘raster’, ‘geoR’
```
```
checking examples ... ERROR
Running examples in ‘spsann-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: optimCORR
> ### Title: Optimization of sample configurations for spatial trend
> ###   identification and estimation
> ### Aliases: objCORR optimCORR
> ### Keywords: iteration optimize spatial
> 
> ### ** Examples
> 
> require(sp)
Loading required package: sp
> data(meuse.grid)
> candi <- meuse.grid[, 1:2]
> covars <- meuse.grid[, 5]
> set.seed(2001)
> ## Not run: 
> ##D # This example takes more than 5 seconds to run!
> ##D res <- optimCORR(points = 100, candi = candi, covars = covars,
> ##D                  use.coords = TRUE)
> ##D objSPSANN(res) # 0.06386069
> ##D objCORR(points = res, candi = candi, covars = covars, use.coords = TRUE)
> ## End(Not run)
> # Random sample
> pts <- sample(1:nrow(candi), 5)
> pts <- cbind(pts, candi[pts, ])
> objCORR(points = pts, candi = candi, covars = covars, use.coords = TRUE)
Error: 'is.any.factor' is not an exported object from 'namespace:pedometrics'
Execution halted
```

Given the significant change to ***pedometrics***, I do believe most of these
errors to be a result of changes to ***pedometrics***. The maintainer of the 
only downstream dependence of ***pedometrics*** has been notified about the
changes to ***pedometrics*** and will make the needed changes ASAP.
