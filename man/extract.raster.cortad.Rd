\name{extract.raster.cortad}
\alias{extract.raster.cortad}
\title{
CoRTAD's raster extraction
}
\description{
This function crops a multi-layer raster object specified by a variable name and an extent object and then calculates values using a formula.
}
\usage{
extract.raster.cortad(nc, var, ext, fxn)
}
\arguments{
  \item{nc}{
a character vector of the destination of CoRTAD's .nc file
}
  \item{var}{
a character vector of the variable (e.g. 'WeeklySST')
}
  \item{ext}{
a vector of four numbers representing the extent (i.e. xmin, xmax, ymin, ymax)
}
  \item{fxn}{
a function (e.g. 'mean', 'min', 'max', 'sd')
}
}

\value{
a Raster object
  }
\examples{

rs <- extract.raster.cortad(nc = "cortadv6_WeeklySST.nc",
                            var = "WeeklySST",
                            ext = c(47 , 57, 22.9, 30.9),
                            fxn = mean)
