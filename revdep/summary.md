# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.2.1 (2015-06-18) |
|system   |x86_64, linux-gnu            |
|ui       |RStudio (0.99.467)           |
|language |en_GB:en                     |
|collate  |en_GB.UTF-8                  |
|tz       |NA                           |

## Packages

|package      |*  |version |date       |source         |
|:------------|:--|:-------|:----------|:--------------|
|car          |*  |2.0-25  |2015-03-03 |CRAN (R 3.2.1) |
|gstat        |*  |1.0-25  |2015-06-19 |CRAN (R 3.2.1) |
|latticeExtra |*  |0.6-26  |2013-08-15 |CRAN (R 3.2.1) |
|moments      |   |0.14    |2015-01-05 |CRAN (R 3.2.1) |
|pbapply      |*  |1.1-1   |2014-05-16 |CRAN (R 3.2.1) |
|plyr         |*  |1.8.3   |2015-06-12 |CRAN (R 3.2.1) |
|Rcpp         |*  |0.12.0  |2015-07-25 |CRAN (R 3.2.1) |
|sp           |*  |1.1-1   |2015-06-05 |CRAN (R 3.2.1) |
|SpatialTools |*  |0.5.8   |2014-07-02 |CRAN (R 3.2.1) |
|spsurvey     |*  |3.0     |2015-05-22 |CRAN (R 3.2.1) |
|xtable       |*  |1.7-4   |2014-09-12 |CRAN (R 3.2.1) |

# Check results
1 checked out of 1 dependencies 

## spsann (1.0.0)
Maintainer: Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>

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
```
DONE
Status: 1 ERROR, 2 NOTEs
```

