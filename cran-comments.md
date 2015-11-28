## Changes
* New functions have been added. They are designed for three purposes:
    + Variogram estimation and analysis
    + Evaluation of geostatistical models of uncertainty
    + Calibrating random forest models

## Test environments
* ubuntu 12.04 (on travis-ci), R 3.2.2
* x86_64-pc-linux-gnu (ubuntu 14.04), R version 3.2.2 (2015-08-14)
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTEs (win-builder):

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>’

Possibly mis-spelled words in DESCRIPTION:
  Pedometric (4:8)
  pedometrics (6:14)
```
```
* checking package dependencies ... NOTE
  No repository set, so cyclic dependency check skipped
```

## Downstream dependencies
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

