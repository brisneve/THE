\name{THE sample datasets}
\alias{crw}
\alias{cortad}
\docType{data}
\title{
Sample CWR and CoRTAD time series datasets
}
\description{
}
\usage{data("crw")}
\format{
  A data frame with 2144 observations on the following 8 variables.
  \describe{
    \item{\code{date}}{a Date}
    \item{\code{SST}}{a numeric vector of 90 percentile of Sea Surface Temperature}
    \item{\code{min_SST}}{a numeric vector of minimum Sea Surface Temperature}
    \item{\code{max_SST}}{a numeric vector of maximum Sea Surface Temperature}
    \item{\code{SSTA}}{a numeric vector of SST Anomaly}
    \item{\code{TSA}}{a numeric vector of Thermal Stress Anomaly}
    \item{\code{DHW}}{a numeric vector of Degree Heating Weeks}
    \item{\code{source}}{a character vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{

NOAA Coral Reef Watch. 2019, updated daily. NOAA Coral Reef Watch Version 3.0 Daily Global 5-km Satellite Virtual Station Time Series Data for Bahrain, Mar. 12, 2013-Jan. 25, 2019. College Park, Maryland, USA: NOAA Coral Reef Watch. Data set accessed 2018-02-05 at https://coralreefwatch.noaa.gov/vs/data.php.

}
\examples{
data(crw)
str(crw)
