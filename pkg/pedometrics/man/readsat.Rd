\name{readsat}
\alias{readsat}
\title{
Read satellite imagery
}
\description{
This function reads satellite imagery and saves it in the native GRASS 6+ format in the current GRASS 6+ LOCATION. Three importing options are available: single file with single band, single file with group of bands, and multiple files with a single band.
}
\usage{
readsat(import = "single", dir = getwd(), overwrite = FALSE, input,
        output, sat, in_prefix, in_suffix, out_prefix)
}
\arguments{
  \item{import}{type of file(s) being imported. Three importing options 
    are available: single file with single band
    (\code{import = "single"}), single file with group of bands
    (\code{import = "group"}), and multiple files with a single band
    (\code{import = "multi"}). Defaults to \code{import = "single"}.}
  
  \item{dir}{directory where the files to be imported are stored.}
    
  \item{overwrite}{allow output files to ovewrite existing files.
    Defaults to \code{overwrite = FALSE}. Obs.: does not to work when
    \code{import = "group"}.}
    
\emph{Importing a single file with one or more bands:}

  \item{input}{name of the file to be imported. This can be a file
    containing a single band (\code{import = "single"}) or a group of
    bands (\code{import = "group"}).}
    
  \item{output}{name for output file(s). When importing a file with
    a single band (\code{import = "single"}), this sets the full name
    of the output file. When importing a file with a group of bands 
    (\code{import = "group"}), this sets the prefix of output files
    name, which is followed by the band number.}
    
\emph{Importing multiple files (\code{import = "multi"}):}

  \item{sat}{satellite and sensor that generated the data stored in the
    multiple files being imported. Currently accepts only
    \code{sat = "landsat5tm"} and \code{sat = "landsat7etm"}.}
    
  \item{in_prefix}{prefix of input files name. See \sQuote{Details}.}
  
  \item{in_suffix}{suffix of input files name. See \sQuote{Details}.}
  
  \item{out_prefix}{prefix of output files name. Defaults to 
    \code{out_prefix = in_prefix}. See \sQuote{Details}.}
}
\details{
When importing multiple files with a single band, file names are automatically identified by \code{readsat()} using information provided with arguments \code{sat}, \code{in_prefix} and \code{in_suffix}. Argument \code{sat} provides the number of bands that are (or should be) stored in the given directory. \code{readsat()} expects that the file name contains at some place of its whole extension a numerical character informing the band number. For example, \sQuote{2010dez26_BAND1.tif} is the name of a *.tif file which contains the band number 1 of Landsat 5 TM satellite, which was obtained on December, 10th of 2010. \code{in_prefix} corresponds to the group of characters preceding the band number. In this example, \code{in_prefix = 2010dez26_BAND}. On the order hand, \code{in_suffix} corresponds to the group of characters that come after the band number. In this example, \code{in_suffix = .tif}. The default is to name output files using a slight modification of the original name, which includes using the band number as file extension (\sQuote{2010dez26_BAND.1}). Currently \code{readsat()} supports setting only a different \code{out_prefix}. Information about each file after reading them and saving in the native GRASS 6+ format can be retrieved using \code{execGRASS("r.info", parameters = list(map = "2010dez26_BAND.1"))}.
}
\value{
This function returns the information about its progress while reading the files from the specified directory. All files are saved in the native GRASS 6+ format in the current GRASS 6+ LOCATION.
}
\references{
GRASS DEVELOPMENT TEAM (2012) \emph{Geographic Resources Analysis Support System (GRASS) Software}.

Neteler, M. and Mitasova, H. (2008) \emph{Open source GIS - a GRASS GIS approach}. New York: Springer, 406p.

}
\author{
Alessandro Samuel-Rosa \email{alessandrosamuelrosa@gmail.com}
}
\note{
Adapted from the the GRASS GIS 6+ function \code{r.in.gdal} authored by Frank Warmerdam (\email{warmerdam@pobox.com}).
}
\section{Warnings}{
Argument \code{overwrite = FALSE} does not to work when \code{import = "group"}.
}
\section{TODO}{
  \itemize{Add option to harmonize raster resolution - Landsat thermal band.}
  \itemize{Function \code{writesat()} to write satellite imagery using GRASS 6+, with options to single file and group of files (stack in raster package).}
}
\seealso{
\code{\link[spgrass6]{readRAST6}}, \code{\link[spgrass6]{writeRAST6}}
}
\examples{
\dontrun{
## read files
## single file with single band
readsat(import = "single", input = "BAND1.tif", output = "band1")

# single file with group of bands
readsat(import = "group", input = "landsat5tm.tif",
        output = "landsat5tm")

# multiple files
readsat(import = "multi", dir = "/home/data/satelite",
        sat = "landsat5tm", in_prefix = "landsat5tm_band",
        in_suffix = ".tif", out_prefix = "landsat5tm_band")
}
}
\keyword{manip}%RShowDoc("KEYWORDS")
\keyword{classes}