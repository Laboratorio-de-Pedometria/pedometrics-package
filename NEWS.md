# Version 0.6-3 (2015-07-27)
* Packaging:
    + Now using Travis to check the package after every commit;
    + Functions from non-default packages are imported to pass CRAN checks;
* New functions:
    + Evaluation of the data type contained in an object;
    + Extract spatial trend data from an object of class `likfit` (see the geoR
      package for more info): `trend.terms()` and `trend.matrix()`;
    + Stratification and categorization of continuous data: `breakPoints()` and
      `stratify()`;
    + Computation of lag-distance classes for varioram estimation: `vgmLags()`;
* Existing functions:
    + `cramer()`: `NA`s are now removed when computing the chi-squared 
      statistic;
    + `plotHD()`: if the Box-Cox transformation is required, the data is now
      first checked for zeros and negative values. If zeros and negative values
      are present, the data is transformed into a positive-valued data, with 
      the minimum value equal to 1.
    + `plotESDA()`: new argument to define the lag-distance classes.

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
