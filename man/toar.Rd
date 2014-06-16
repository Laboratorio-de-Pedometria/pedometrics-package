\name{toar}
\alias{toar}
\title{
Top-of-atmosphere radiance (or reflectance) and temperature
}
\description{
Calculates top-of-atmosphere radiance (or reflectance) and temperature for Landsat sensors. Algorithm parameters are automatically extracted from the XML metadata file.
}
\usage{
toar(in_prefix, out_prefix, sat, meta, correct = TRUE, rad = TRUE)
}
\arguments{
  \item{in_prefix}{prefix of input files name. A character string
    giving all characters preceeding the band number, which is
      expected to be the file extension. See \sQuote{Details}.}
  \item{out_prefix}{prefix of output files name. See
    \sQuote{Details}.}
  \item{sat}{satellite and sensor that generated the data. See
    \sQuote{Details}.}
  \item{meta}{metadata. A character string giving the path to the XML
    metadata file. Because the content of metadata files is the same
    for all bands, any of them can be used.}
  \item{correct}{whether to correct for negative values. Defaults to
    \code{correct = TRUE}. See \sQuote{Details}.}
  \item{rad}{output value. Defines if the output value should be
    at-sensor radiance or reflectance for all bands. Defaults to
    radiance (\code{rad = TRUE}).}
}
\details{
This function was adapted from the GRASS 6+ function \code{i.landsat.toar}. Only a few of its capabilities were implemented because many of them are unnecessary at present, such as the dark object subtraction method (DOS). Future releases of \pkg{pedometrics} can include other functionalities if necessary.
\subsection{supported_sensors}{
Currently \code{toar()} supports the following Landsat sensors:\cr
Landsat 1 MSS (\code{landsat1mss})\cr
Landsat 2 MSS (\code{landsat2mss})\cr
Landsat 3 MSS (\code{landsat3mss})\cr
Landsat 4 MSS (\code{landsat4mss})\cr
Landsat 5 MSS (\code{landsat5mss})\cr
Landsat 4 TM  (\code{landsat4tm})\cr
Landsat 5 TM  (\code{landsat5tm})\cr
Landsat 7 ETM (\code{landsat7etm+})\cr
Landsat 8 OLI/TIRS (\code{landsat8oli/tirs})\cr
\code{toar()} reads satellite sensor information from the metadata file provided with argument \code{meta}. The argument \code{sat} is used for sanity, that is, to ensure that the metadata file specified contains the correct information. If satellite sensor information provided within the arguments list does not match that of the metadata file, \code{toar()} stops and suggests the user to check both the arguments list and the metadata file.
}
\subsection{file_name}{
The name of each input file is automatically identified by \code{toar()} using the information provided with argument \code{in_prefix}. This is a character string with all characters preceding the band number in the file name. If input files were read and saved into the current GRASS 6+ LOCATION using \code{readsat()} then they share the same \code{in_prefix}. The only difference is the file extension, which is set to be equal to the band number. For example: \sQuote{2010dez26_BAND.1}. In this case, \code{in_prefix = "2010dez26_BAND."}. If the desired output file name is, for example, \sQuote{2010dez26_toar.1}, then \code{out_prefix = "2010dez26_toar."}. Again, the band number will be used as file extension. It is important to note that \code{toar()} requires the file extension to be equal to the band number. This is because the flag \code{-n} is used in the source code and cannot be modified by the user. If the file extension is any but the band number, \code{toar()} will not complete the task.
}
\subsection{\code{correct}}{
At-sensor reflectance values range from zero to one, whereas at-sensor radiance must be greater or equal to zero. However, since Lmin can be a negative number then the at-sensor values can also be negative. To avoid these possible negative values and set the minimum possible values at-sensor to zero, setting \code{correct = TRUE} corrects the radiance to output a corrected at-sensor values using the equations (not for thermal bands):

\code{radiance = (uncorrected_radiance - Lmin)}

\code{reflectance = radiance / sun_radiance}
}
}
\value{
GRASS 6+ raster files with top-of-atmosphere radiance (or reflectance) and temperature for all bands. All processing steps are printed during execution.
}
\references{
Chander, G., Markham, B.L. and Helder, D.L. (2009) Summary of current radiometric calibration coefficients for Landsat MSS, TM, ETM+, and EO-1 ALI sensors. \emph{Remote Sensing of Environment}, v. 113, p. 893-903.

Chander, G. and Markham, B. (2003) Revised Landsat-5 TM radiometric calibration procedures and post calibration dynamic ranges. \emph{IEEE Transactions on Geoscience and Remote Sensing}, v. 41, p. 2674-2677.

P.S., Chavez Jr. (1996) Image-based atmospheric corrections - Revisited and Improved. \emph{Photogrammetric Engineering and Remote Sensing}, v. 62, n. 9, p. 1025-1036.

Huang et al (2002) At-Satellite Reflectance, 2002: A First Order Normalization Of Landsat 7 ETM+ Images.

Irish, R. (2011) Landsat 7. Science Data Users Handbook.

B.L., Markham and Barker, J.L. (1986) Landsat MSS and TM Post-Calibration Dynamic Ranges, Exoatmospheric Reflectances and At-Satellite Temperatures. \emph{EOSAT Landsat Technical Notes}, No. 1.

Moran, M., Jackson, R.D., Slater, P.N. and Teillet, P.M. (1992) Evaluation of simplified procedures for retrieval of land surface reflectance factors from satellite sensor output. \emph{Remote Sensing of Environment}, v. 41, p. 169-184.

Song, C., Woodcock, C.E., Seto, K.C., Lenney, M.P. and Macomber, S.A. (2001) Classification and Change Detection Using Landsat TM Data: When and How to Correct Atmospheric Effects? \emph{Remote Sensing of Environment}, v. 75, p. 230-244.
}
\author{
Alessandro Samuel-Rosa \email{alessandrosamuelrosa@gmail.com}
}
\note{
Adapted from the the GRASS 6+ function \code{i.landsat.toar} authored by E. Jorge Tizado \email{ej.tizado@unileon.es}.

This function was tested only for Landsat 5 TM imagery downloaded from INPE-DGI.
}
\seealso{
\code{\link[spgrass6]{execGRASS}}, \code{\link[pedometrics]{readsat}}
}
\examples{
\dontrun{
## update region definitions
execGRASS("g.region", parameters = list(rast = "1986jan06_BAND.1"))

## run toar
toar(in_prefix = "1986jan06_BAND.", out_prefix = "1986jan06_toar.",
     method = "corrected", meta = "1986jan06_BAND1.xml", rad = FALSE)

## list raster files
system(paste("g.list rast"))

## print file info
execGRASS("r.info", parameters = list(map = "1986jan06_toar.1"))
}
}
\keyword{methods}%RShowDoc("KEYWORDS")