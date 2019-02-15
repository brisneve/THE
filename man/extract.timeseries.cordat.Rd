\name{extract.timeseries.cordat}
\alias{extract.timeseries.cordat}
\title{
Extraction of variable from CoRDAT .nc files
}
\description{
Extracts variable from CoRDAT .nc files
}
\usage{
extract.cordat(nc, varname, coor, fun = mean, method = "bilinear", buffer = 5000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nc}{
destination of the .nc files
}
  \item{varname}{
name of the variable to be extraction from .nc files
}
  \item{coor}{
vector/matrix of mid longitude and latitude
}
  \item{method}{
See the argument method in extract function
}
  \item{buffer}{
See the argument buffer in extract function; default is set to 5000
}
}
