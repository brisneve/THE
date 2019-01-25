\name{clean.timeseries}
\alias{clean.timeseries}
\title{
Housekeeping time series
}
\description{
This function creates a  time series of observation with fixed continuous intervals and no missing values. Useful in merging and harmonising two time series with different intervals and have missing values.
}
\usage{
harmonise.timeseries(x,y, start_date, end_date, by = "week")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  date of observations (class: date, format: YYYY-MM-DD)
}
  \item{y}{
  estimate of observations (class: numeric, format: YYYY-MM-DD)
}
  \item{start_date}{
  start date of observations (class: date, format: YYYY-MM-DD)
}
 \item{end_date}{
  end date of observations (class: date, format: YYYY-MM-DD)
}
 \item{by}{
  interval, e.g. day, week, month, year; default = "week"
}
}

\value{
This function returns to a data frame
  \item{date }{date of observations in fixed interval}
  \item{estimate}{estimate of observation in fixed interval}
}
