\name{compute.the}
\alias{compute.the}
\title{
Thermal Stress Metrics
}
\description{
This function calculates thermal stress metrics for coral bleaching.
}
\usage{
compute.the(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
date of observation (class: date, format: YYYY-MM-DD)
}
  \item{y}{
estimate of observation (class: numeric)
}
  \item{z}{
interval of climatology; either week or month; default is week
}
}

\value{
This returns to a data frame of
  \item{date }{date of observations, format YYYY-MM-DD}
  \item{SST }{Sea surface temperature in deg C}
  \item{climatology }{Long-term monthly mean SST in deg C}
  \item{SSTA}{Sea surface temperature Anomaly in deg C}
  \item{TSA}{Thermal stress anomaly in deg C}
  \item{DHW}{Degree heating weeks in deg C - weeks}
}
