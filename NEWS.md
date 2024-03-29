# News

## pedometrics 0.12.1

* __Bug fix__ Remove escaped LaTeX specials in the documentation of `plotESDA()`. These escaped
  LaTeX specials were resulting in notes in CRAN package check results for some flavors.

## pedometrics 0.12.0

* __Improvement 1__ New function `skewness()`: compute the moment coefficient of skewness of a
  continuous, possibly non-normal variable.
* __Improvement 2__ Reducing dependencies:
  * Package __moments__ is not in Suggests anymore. The moment coefficient of skewness used in
    `plotHD()` is now computed using the new function `skewness()`.
  * Package __plyr__ is not Suggests anymore.
  * Package __geoR__ is not Suggests anymore.
* __Improvement 3__ Extended documentation. Some __pedometrics__-functions depend on functions
  coming from other R-packages. These dependencies are now stated in the function help. This should
  enable users to get ready before missing-package warnings are issued. Links to old and
  development versions of those are provided as well. This should help users when a package is
  temporarily removed from CRAN -- such as with __geoR__ and __georob__.
* __Improvement 4__ DESCRIPTION.
  * The package description was extended to state which tools are included in the package.
  * The CRAN archive <https://cran.r-project.org/src/contrib/Archive/> is referenced so that users
    can find formerly available versions of suggested packages that are no longer available from
    CRAN because they were temporarily removed -- such as with __geoR__ and __georob__.
  * Contributors of defunct functions were moved to `pedometrics-defunct.R`.

## pedometrics 0.11.1

* __Bug fixes__
  * The documentation of a few functions was missing the returned value. This was fixed. Now all
    functions have the returned value clearly specified.
  * The documentation of a few functions was using `\dontrun{}`. This was fixed. Now all functions
    have at least one example that can be run conditionally on the availability of certain packages
    or if R is being used interactively.
* __Improvement 1__ Function documentation was cleaned up.
* __Improvement 2__ In previous versions, `plotCor()` required `RColorBrewer::brewer.pal()` to
    set the color palette. Now the color palette is hard-coded and package __RColorBrewer__ was
    thus removed from _Suggests_.
* __Improvement 3__ Add `Additional_repositories` to _DESCRIPTION_ as a means to deal with archived
    packages such as __geoR__ and __georob__.

## pedometrics 0.11.0

* __Improvement__ Run local and remote tests. Prepare comments to CRAN maintainers.

## pedometrics 0.10.1

* __Bug fix__ Fixed mistaken use of a Suggested package in an example unconditionally on its
  availability.
* __Improvement__ Run local and remote tests. Prepare comments to CRAN maintainers.

## pedometrics 0.10.0

* __Improvement__ Run local and remote tests. Prepare comments to CRAN maintainers.

## pedometrics 0.9.0

* __Improvement__ Functions depending on the __spsurvey__ package were causing problems. These
  problems occurred due to the changes introduced in version 5.0.0 of the __spsurvey__ package. The
  most efficient solution was to move those functions to the __ASRtools__ package: `cdfPlot()`,
  `cdfStats()`, `cdfTable()`, and `coordenadas()`. As a result, packages __spsurvey__ and __xtable__
  were removed from Suggests. The __ASRtools__ package is available at
  https://github.com/samuel-rosa/ASRtools.

## pedometrics 0.8.2

* __Improvement__ Expanded the documentation and cleaned the code of various functions.

## pedometrics 0.8.1

* __Improvement__ Expanded documentation on installing the development version from GitHub.

## pedometrics 0.8.0

* __Improvement 1__ The __spsurvey__ package moved to Suggests. A major update to version 5.0
  of the __spsurvey__ package is to be released soon. This major release will introduce
  breaking changes to previously existing functions. To avoid these breaking changes to have
  immediate effect on the __pedometrics__ package, it now Suggests spsurvey (< 5.0).
* __Improvement 2__ We are working to reduce the number of dependencies as well as of the suggested
  packages. This is positive for users. From this version on, the __pbapply__ package is not
  suggested any more.
* __Bug fix.__ Fixes CRAN check note on r-devel-linux-x86_64-fedora-clang due to undeclared packages
  in Rd xrefs.

## pedometrics 0.7.0.9000 (2020-02-09)

* `plotHD` and `stepVIF`: reviews documentation and plans future changes.

## pedometrics 0.7.0 (2020-02-08)

* New version (minor), including bug fixes, new functions, and function improvements.

## pedometrics 0.6.6.9010 (2020-02-08)

* Improvements:
  * Reviews and improves documentation.
* Deprecated functions: `coordenadas()`, `cdfTable()`, `cdfStats()` and `cdfPlot()`. These functions
  were developed to fulfill the demands created by the PhD research project (2012-2016) and are no
  longer (or rarely) used. Some will be replaced, but all will move to a package currently under
  developed at <https://github.com/samuel-rosa/ASRtools>.

## pedometrics 0.6-6.9009 (2020-02-03)

* Improvements:
  * Reviews and improves documentation.
* Fixes:
  * Deals with the fact that matrix objects now also inherit from class "array".
  * Deals with the fact that the suggested package __geoR__ has been orphaned on 2020-01-12. This is
    done by keeping the package in `Suggests` and using it conditionally by employing
    `requireNamespace()`.

## pedometrics 0.6-6.9008 (2019-01-31)

* Improvements:
  * `plotCor`: reverses default color ramp so that positive and negative correlations are depicted
    using red and blue colors, respectively.

## pedometrics 0.6-6.9007 (2019-01-22)

* Improvements:
  * `plotHD`: adds grid lines to histogram.
  * `plotESDA`: choose the position of the legend of the bubble plot

## pedometrics 0.6-6.9006 (2019-01-21)

* Improvements on `plotCor` to deal with long column names in the correlation matrix. Long column
  names (> 5 characters) are now replaced with x1, x2, ..., xn, where n is the number of columns in
  the correlation matrix. A message is issues identifying columns and their respective index.

## pedometrics 0.6-6.9005 (2019-01-15)

* Adds `plotCor`, a function to create correlation plots using a colorblind-friendly palette.

## pedometrics 0.6-6.9002 (2018-02-23)

* Evaluates the upcoming new version (3.0-0) of the **car** package.

## pedometrics 0.6-6.9001 (2015-12-21)

* Improved selection of the location to place the legend in `plotHD`.

## pedometrics 0.6-6.9000 (2015-12-10)

* A new guess of `p_sill` is used when the variance is spatially uncorrelated.
  It is defined as 1e-3 times the guess of `nugget` (before the `p_sill` was
  set to 1e-12, which caused errors in `optim` and `nlminb`).

## pedometrics 0.6-6 (2015-12-03)

* FIX: corrected error in `vgmICP` caused by the recent update of package
  ***georob***.
* `vgmICP`: Improved estimation of the scale parameter (range) for monotonous
  variograms by using function `practicalRange` from package ***geoR***.

## pedometrics 0.6-5.9001 (2015-12-02)

* `vgmICP`: Improve the guess of the partial sill for a pure nugget effect
  model.

## pedometrics 0.6-5.9000 (2015-12-01)

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

## pedometrics 0.6-5 (2015-11-30)

* FIX: the computation of the number of point-pairs per lag-distance class in
  `vgmLags` was incorrect because it neglected the fact that, in a full distance
  matrix, two points *a* and *b* form two pairs, i.e. *ab* and *ba*. The mistake
  is due to the fact that we use `SpatialTools::dist1` to compute the distance
  matrix instead of `stats::dist`.

## pedometrics 0.6-4 (2015-11-28)

* New functions have been added. They are designed for three purposes:
  * Variogram estimation and analysis
  * Evaluation of geostatistical models of uncertainty
  * Calibrating random forest models

## pedometrics 0.6-3.9003 (2015-11-28)

* New function `vgmICP` to guess the initial covariance parameters when fitting
  a variogram model.
* Included description of funding agencies in package description.

## pedometrics 0.6-3.9002 (2015-11-27)

* Improved documentation of the `vgmLags`-function, and added functionality to
  count the number of points or point-pairs per lag-distance class so that the
  user can decide if the returned lag-distance classes meet his/her needs.
* New function `vgmSCV` to compute the proportion of the variance that is
  spatially correlated.
* Minor improvement of the documentation of almost all functions.

## pedometrics 0.6-3.9001 (2015-11-05)

* Developed a new function `checkGMU` which can be used to check the quality
  of a geostatistical model on uncertainty.

## pedometrics 0.6-3.9000 (2015-09-25)

* New functions:
  * Iteratively de-bias random forest regression.

## pedometrics 0.6-3 (2015-07-28)

* Packaging:
  * Now using Travis-CI to check the package after every commit;
  * Functions from default packages other than `base` are now imported to
    comply with the new change to the CRAN policy described at
    <https://developer.r-project.org/blosxom.cgi/R-devel/NEWS/2015/06/29#n2015-06-29>;
  * Most packages that used to be under the `Imports:` field in the
    `DESCRIPTION` file now are under the `Suggests:` field;
* Existing functions:
  * `cramer()`: `NA`s are now removed when computing the chi-squared
    statistic;
  * `plotHD()`: if the Box-Cox transformation is required, the data is now
    first checked for zeros and negative values. If zeros and negative values
    are present, the data is transformed into a positive-valued data, with
    the minimum value equal to 1.
  * `plotESDA()`: new argument to define the lag-distance classes;
* New functions:
  * Evaluation of the data type contained in an object;
  * Extract spatial trend data from an object of class `likfit` (see the geoR
    package for more info): `trend.terms()` and `trend.matrix()`;
  * Stratification and categorization of continuous data: `breakPoints()` and
    `stratify()`;
  * Computation of lag-distance classes for variogram estimation: `vgmLags()`.

## pedometrics 0.6-2 (2015-06-01)

* Better documentation for several functions.
* `cramer()`: the cross-tabulation is now computed using `table()` instead of
  `bigtabulate()` to avoid dependency problems in Windows.

## pedometrics 0.6-1 (2015-02-18)

* Functions to evaluate the data type contained in an object.

## pedometrics 0.6-0 (2015-02-05)

* Migrated from R-Forge to GitHub;

## pedometrics 0.5-1 (2014-01-11)

* Test for objects of type `"numeric"` and `"integer`.

## pedometrics 0.5-0 (2014-01-11)

* Functions used to optimize sample patterns were moved to the new
  package **spsann**: `spJitter()`, `spSANN()`, `spsannMSSD()`, `spsannPPL()`,
  and `spsannCLHS()`.

## pedometrics 0.4-2 (2014-12-08)

* Function to compute the Cramer's V, a measure of association between
  categorical variables.

## pedometrics 0.4-1 (2014-12-01)

* Documentation is now generated using roxygen2.

## pedometrics 0.4-0 (2014-11-26)

* Started the fourth season of the initial development of the package.
  Many functions are being implemented in C++ to speed-up the computation.

## pedometrics 0.3-1 (2014-11-23)

* Improving the spatial simulated annealing.

## pedometrics 0.3-0 (2014-11-15)

* Started the third season of the initial development of the package.
  Significant changes were made in functions `spSANN()`, `spJitter()`, and
  those used as objective functions in spatial simulated annealing.

## pedometrics 0.2-4 (2014-10-07)

* Spatial simulated annealing is now available: `spSANN()`.

## pedometrics 0.2-3 (2014-10-07)

* Corrected bugs in functions `objPairs()`, `objPoints()`, `pairs_per_lag()`,
  `points_per_lag()` and `spJitter()`. Added new criterion to `objPairs()` and
  `objPoints()`. Added new method to `spJitter()` (finite set of candidate
  locations). Most of the changes speed up the functions and increase
  their functionalities.

## pedometrics 0.2-2 (2014-10-02)

* Included functions `objPairs()`, `objPoints()`, `pairs_per_lag()` and
  `points_per_lag()`. These functions were designed to be used with spatial
  simulated annealing.

## pedometrics 0.2-1 (2014-09-30)

* Included functions `spJitter()` and `bbox2sp()`.

## pedometrics 0.2-0 (2014-09-25)

* Started the second season of the initial development of the package.
  Included functions `plotHD()` and `plotESDA()`.

## pedometrics 0.1-9 (2014-08-01)

* Removed `toar()` and `readsat()` because they are better
  implemented using GRASS GIS commands directly.

## pedometrics 0.1-8 (2014-07-03)

* Second public release (R-Forge).
* Included function `buildMS()`, used to build a series of linear
  models using automated variable selection. Included function
  `statsMS()`, used to obtain performance statistics of a series
  of linear models. Included function `plotMS()`, used to plot the
  statistics of a models series. Corrected bugs detected during the
  first built.

## pedometrics 0.1-7 (2014-06-16)

* First public release (R-Forge).
* Several corrections in the documentation, preparing for the first public
  release.

## pedometrics 0.1-6 (2014-05-25)

* Seventh internal release.
* Function `cdfTable()`: added argument to return a data.frame object.

## pedometrics 0.1-5 (2014-05-21)

* Sixth internal release.
* Function `cdfPlot()`: added argument to define if the confidence limits of
  the CDF should be included in the plot.

## pedometrics 0.1-4 (2014-05-20)

* Fifth internal release.
* Function `cdfPlot()` now has options to show CDF parameters.

## pedometrics 0.1-3 (2014-04-21)

* Fourth internal release.
* Included function `stepVIF()`. Renamed many of the functions to comply with
  the style guide. The package was also renamed to **pedometrics**.

## pedometrics 0.1-2 (2014-02-13)

* Third internal release.
* Included functions `readsat()` and `toar()`.

## pedometrics 0.1-1 (2014-01-23)

* Second internal release.
* Minor correction in function `deltagcp()`.

## pedometrics 0.1-0 (2013-06-12)

* First internal release.
* Six functions available: `cdfplot()`, `cdfstats()`, `cdftable()`,
  `coordenadas()`, `deltagcp()`, and `gcpvector()`.
