\name{compute.tsm}
\alias{compute.tsm}
\title{
Thermal Stress Metrics
}
\description{
This function calculates thermal stress metrics for coral bleaching.
}
\usage{
compute.tsm(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
date of observation (class: date, format: YYYY-MM-DD)
}
  \item{y}{
estimate of observation (class: numeric)
}
}

\value{
This returns to a data frame of
  \item{month }{a numeric vector of Month}
  \item{date }{date of observations, format YYYY-MM-DD}
  \item{SST }{Sea surface temperature in deg C}
  \item{monthly_mean }{Long-term monthly mean SST in deg C}
  \item{max_climatology}{Annual maximum climatology in deg C}
  \item{min_climatology}{Annual minimum climatology in deg C}
  \item{mean_climatology}{Annual mean climatology in deg C}
  \item{SSTA}{Sea surface temperature Anomaly in deg C}
  \item{TSA}{Thermal stress anomaly in deg C}
  \item{DHW}{Degree heating weeks in deg C - weeks}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
