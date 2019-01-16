# pedometrics: Miscellaneous Pedometric Tools

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![packageversion](https://img.shields.io/badge/devel%20version-0.6.6.9004-firebrick.svg?style=flat-square)](commits/master)
[![Build Status](https://travis-ci.org/samuel-rosa/pedometrics.svg?branch=master)](https://travis-ci.org/samuel-rosa/pedometrics)
[![CRAN](https://www.r-pkg.org/badges/version/pedometrics)](https://cran.r-project.org/package=pedometrics)
[![cran checks](https://cranchecks.info/badges/worst/pedometrics)](https://cran.r-project.org/web/checks/check_results_pedometrics.html)
[![Rdoc](http://www.rdocumentation.org/badges/version/pedometrics)](http://www.rdocumentation.org/packages/pedometrics)
[![Downloads](http://cranlogs.r-pkg.org/badges/pedometrics?color=brightgreen)](http://www.r-pkg.org/pkg/pedometrics)

The __pedometrics__ package was originally created to share the developments of my PhD research project 
entitled _Contribution to the Construction of Models for Predicting Soil Properties_ (2012-2016) carried out
under the supervision of Dr Lucia HC Anjos (Universidade Federal Rural do Rio de Janeiro, Brazil), Dr Gustavo 
M Vasques (Embrapa Solos, Brazil), and Dr Gerard BM Heuvelink (ISRIC -- World Soil Information, the 
Netherlands). The idea of creating a package came from an observation that was bothering me for some time: 
several of the pedometric methods that I saw on published scientific papers were not being broadly employed as
I would expect. But why? The answer seemed straightforward: because they were not available as a computer
program ready to be used. This was also making it difficult to reproduce the analyses carried out by others. 
Creating a generic package for R -- a popular programing language among pedometricians -- could be a solution.

The current __pedometrics__ package is an implementation of miscellaneous functions for various pedometric
purposes. This includes the calibration of multiple linear regression models, the computation of summary
validation statistics, the generation of plots, the evaluation of the local quality of a geostatistical model 
of uncertainty, and so on. Other functions simply extend the functionalities of or facilitate the usage of
functions from other packages that are commonly used for the analysis of pedometric data. Among these, one of
the functions that I find very useful is `plotESDA()`. This function creates four plots for exploratory spatial
data analysis (ESDA): a histogram + density plot, a bubble plot, a variogram plot, and a variogram map. The 
figure below shows the result for the zinc concentration in the Meuse river data set.

![](https://raw.githubusercontent.com/samuel-rosa/pedometrics/master/inst/extdata/plot-esda.png)

## Installation

The package can be installed from [CRAN][cran] using:

```R
install.packages(pkgs = "pedometrics")
```

The development version, available on [GitHub][github], can be installed -- using the `devtools` package -- as
follows:

[cran]: https://CRAN.R-project.org/package=pedometrics
[github]: https://github.com/samuel-rosa/pedometrics

```R
if (!require(devtools)) {
  install.packages(pkgs = "devtools")
}
devtools::install_github(repo = "samuel-rosa/pedometrics")
```

## How to collaborate

We use the *fork & pull* collaborative development model. This means that you have the freedom to make a 
parallel copy -- _fork_ -- of this repository, edit the source code as you see fit and then send -- _push_ --
the changes to your personal coppy of this repository. You can do all this without asking any permission. In
case the changes that you have made in your pesonal copy of this repository are interesting and you have the 
interest of sharing them with us, then all you have to do is require them to be pushed -- _pull request_ -- to 
this repository. After a review of the changes, we will decide if they can be merged -- _merge_ -- with the
source code of this repository.
