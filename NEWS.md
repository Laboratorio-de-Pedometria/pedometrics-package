# Version 0.7.0 (2020-02-08)

* New version (minor) including bug fixes, new functions, and function improvements.

# Version 0.6.6.9010 (2020-02-08)

* Improvements:
  + Reviews and improves documentation.
* Deprecated functions: `coordenadas()`, `cdfTable()`, `cdfStats()` and `cdfPlot()`. These functions were
  developed to fulfill the demands created by the PhD research project (2012-2016) and are no longer (or
  rarely) used. Some will be replaced, but all will move to a package currently under developed at 
  https://github.com/samuel-rosa/ASRtools.

# Version 0.6.6.9009 (2020-02-03)

* Improvements:
  + Reviews and improves documentation.
* Fixes:
  + Deals with the fact that matrix objects now also inherit from class "array".
  + Deals with the fact that the suggested package __geoR__ has been orphaned on 2020-01-12. This is done by 
    keeping the package in `Suggests` and using it conditionally by employing `requireNamespace()`.

# Version 0.6.6.9008 (2019-01-31)

* Improvements:
  - `plotCor`: reverses default color ramp so that positive and negative correlations are depicted using red 
    and blue colors, respectively.

# Version 0.6.6.9007 (2019-01-22)
* Improvements:
  - `plotHD`: adds grid lines to histogram.
  - `plotESDA`: choose the position of the legend of the bubble plot

# Version 0.6.6.9006 (2019-01-21)
* Improvements on `plotCor` to deal with long column names in the correlation matrix. Long column names (> 5 
  characters) are now replaced with x1, x2, ..., xn, where n is the number of columns in the correlation matrix.
  A message is issues identifying columns and their respective index.

# Version 0.6.6.9005 (2019-01-15)
* Adds `plotCor`, a function to create correlation plots using a colorblind-friendly palette.

# Version 0.6.6.9002 (2018-02-23)
* Evaluates the upcoming new version (3.0-0) of the **car** package.

# Version 0.6.6.9001 (2015-12-21)
* Improved selection of the location to place the legend in `plotHD`.

# Version 0.6.6.9000 (2015-12-10)
* A new guess of `p_sill` is used when the variance is spatially uncorrelated. 
  It is defined as 1e-3 times the guess of `nugget` (before the `p_sill` was 
  set to 1e-12, which caused errors in `optim` and `nlminb`).

# Version 0.6.6 (2015-12-03)
* FIX: corrected error in `vgmICP` caused by the recent update of package 
  ***georob***.
* `vgmICP`: Improved estimation of the scale parameter (range) for monotonous
  variograms by using function `practicalRange` from package ***geoR***.

# Version 0.6-5.9001 (2015-12-02)
* `vgmICP`: Improve the guess of the partial sill for a pure nugget effect 
  model.

# Version 0.6-5.9000 (2015-12-01)
* FIX: the definition of the lag-distance classes passed to
  `georob::sample.variogram` in `vgmLags` was incorrect due to an error in the
  documentation the former. Instead of "a numeric vector with the upper bounds 
  of a set of contiguous bins", the argument `lag.dist.def` of
  `georob::sample.variogram` requires "a numeric vector with the *lower and*
  upper bounds of a set of contiguous bins". The author and maintainer of the
  ***georob***-package has been warned about this documentation error.
* Improve guess of nugget variance by giving larger weight to the first lag
  because we now consider a minimum number of point-pairs per lag-distance 
  class.

# Version 0.6-5 (2015-11-30)
* FIX: the computation of the number of point-pairs per lag-distance class in
  `vgmLags` was incorrect because it neglected the fact that, in a full distance
  matrix, two points *a* and *b* form two pairs, i.e. *ab* and *ba*. The mistake
  is due to the fact that we use `SpatialTools::dist1` to compute the distance
  matrix instead of `stats::dist`.

# Version 0.6-4 (2015-11-28)
* New functions have been added. They are designed for three purposes:
    + Variogram estimation and analysis
    + Evaluation of geostatistical models of uncertainty
    + Calibrating random forest models

# Version 0.6-3.9003 (2015-11-28)
* New function `vgmICP` to guess the initial covariance parameters when fitting
  a variogram model.
* Included description of funding agencies in package description.

# Version 0.6-3.9002 (2015-11-27)
* Improved documentation of the `vgmLags`-function, and added functionality to
  count the number of points or point-pairs per lag-distance class so that the
  user can decide if the returned lag-distance classes meet his/her needs.
* New function `vgmSCV` to compute the proportion of the variance that is
  spatially correlated.
* Minor improvement of the documentation of almost all functions.

# Version 0.6-3.9001 (2015-11-05)
* Developed a new function `checkGMU` which can be used to check the quality
  of a geostatistical model on uncertainty.

# Version 0.6-3.9000 (2015-09-25)
* New functions:
    + Iteratively de-bias random forest regression.

# Version 0.6-3 (2015-07-28)
* Packaging:
    + Now using Travis-CI to check the package after every commit;
    + Functions from default packages other than `base` are now imported to
      comply with the new change to the CRAN policy described at
      http://developer.r-project.org/blosxom.cgi/R-devel/NEWS/2015/06/29#n2015-06-29;
    + Most packages that used to be under the `Imports:` field in the
    `DESCRIPTION` file now are under the `Suggests:` field;
* Existing functions:
    + `cramer()`: `NA`s are now removed when computing the chi-squared 
      statistic;
    + `plotHD()`: if the Box-Cox transformation is required, the data is now
      first checked for zeros and negative values. If zeros and negative values
      are present, the data is transformed into a positive-valued data, with 
      the minimum value equal to 1.
    + `plotESDA()`: new argument to define the lag-distance classes;
* New functions:
    + Evaluation of the data type contained in an object;
    + Extract spatial trend data from an object of class `likfit` (see the geoR
      package for more info): `trend.terms()` and `trend.matrix()`;
    + Stratification and categorization of continuous data: `breakPoints()` and
      `stratify()`;
    + Computation of lag-distance classes for variogram estimation: `vgmLags()`.

# Version 0.6-2 (2015-06-01)
* Better documentation for several functions.
* `cramer()`: the cross-tabulation is now computed using `table()` instead of
  `bigtabulate()` to avoid dependency problems in Windows.

# Version 0.6-1 (2015-02-18)
* Functions to evaluate the data type contained in an object.

# Version 0.6-0 (2015-02-05)
* Migrated from R-Forge to GitHub;

# Version 0.5-1 (2014-01-11)
* Test for objects of type `"numeric"` and `"integer`.

# Version 0.5-0 (2014-01-11)
* Functions used to optimize sample patterns were moved to the new
  package **spsann**: `spJitter()`, `spSANN()`, `spsannMSSD()`, `spsannPPL()`,
  and `spsannCLHS()`.

# Version 0.4-2 (2014-12-08)
* Function to compute the Cramer's V, a measure of association between
  categorical variables.

# Version 0.4-1 (2014-12-01)
* Documentation is now generated using roxygen2.

# Version 0.4-0 (2014-11-26)
* Started the fourth season of the initial development of the package.
  Many functions are being implemented in C++ to speed-up the computation.

# Version 0.3-1 (2014-11-23)
* Improving the spatial simulated annealing.

# Version 0.3-0 (2014-11-15)
* Started the third season of the initial development of the package.
  Significant changes were made in functions `spSANN()`, `spJitter()`, and 
  those used as objective functions in spatial simulated annealing.

# Version 0.2-4 (2014-10-07)
* Spatial simulated annealing is now available: `spSANN()`.

# Version 0.2-3 (2014-10-07)
* Corrected bugs in functions `objPairs()`, `objPoints()`, `pairs_per_lag()`,
  `points_per_lag()` and `spJitter()`. Added new criterion to `objPairs()` and
  `objPoints()`. Added new method to `spJitter()` (finite set of candidate
  locations). Most of the changes speed up the functions and increase
  their functionalities.

# Version 0.2-2 (2014-10-02)
* Included functions `objPairs()`, `objPoints()`, `pairs_per_lag()` and 
  `points_per_lag()`. These functions were designed to be used with spatial
  simulated annealing.

# Version 0.2-1 (2014-09-30)
* Included functions `spJitter()` and `bbox2sp()`.

# Version 0.2-0 (2014-09-25)
* Started the second season of the initial development of the package.
  Included functions `plotHD()` and `plotESDA()`.

# Version 0.1-9 (2014-08-01)
* Removed `toar()` and `readsat()` because they are better
  implemented using GRASS GIS commands directly.

# Version 0.1-8 (2014-07-03)
* Second public release (R-Forge).
* Included function `buildMS()`, used to build a series of linear 
  models using automated variable selection. Included function
  `statsMS()`, used to obtain performance statistics of a series
  of linear models. Included function `plotMS()`, used to plot the
  statistics of a models series. Corrected bugs detected during the
  first built.

# Version 0.1-7 (2014-06-16)
* First public release (R-Forge).
* Several corrections in the documentation, preparing for the first public
  release.

# Version 0.1-6 (2014-05-25)
* Seventh internal release.
* Function `cdfTable()`: added argument to return a data.frame object.

# Version 0.1-5 (2014-05-21)

* Sixth internal release.
* Function `cdfPlot()`: added argument to define if the confidence limits of 
  the CDF should be included in the plot.

# Version 0.1-4 (2014-05-20)
* Fifth internal release.
* Function `cdfPlot()` now has options to show CDF parameters.

# Version 0.1-3 (2014-04-21)
* Fourth internal release.
* Included function `stepVIF()`. Renamed many of the functions to comply with 
  the style guide. The package was also renamed to **pedometrics**.

# Version 0.1-2 (2014-02-13)
* Third internal release.
* Included functions `readsat()` and `toar()`.

# Version 0.1-1 (2014-01-23)
* Second internal release.
* Minor correction in function `deltagcp()`.

# Version 0.1-0 (2013-06-12)
* First internal release.
* Six functions available: `cdfplot()`, `cdfstats()`, `cdftable()`, 
  `coordenadas()`, `deltagcp()`, and `gcpvector()`.
