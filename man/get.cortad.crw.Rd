\name{get.cortad.crw}
\alias{get.crw}
\alias{get.cortad}
\alias{get.cortad.crw}
\title{
Extraction of CRW and CoRTAD time series data
}
\description{

These functions extract time series of  sea surface temperature (SST) and related thermal stress metrics from NOAA Coral Reef Watch (CRW) and/or Coral Reef Temperature Anomaly Database (CoRTAD). See references for the further details of these databases.

}
\usage{
get.crw(url,destfile = NULL)

get.cortad(sst,ssta,tsa,lon, lat, method = "bilinear", buffer = 5000, destfile = NULL)

get.cortad.crw(url,sst,ssta,tsa, method = "bilinear", buffer = 5000,destfile = NULL)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{url}{
URL of the file to download from CRW database
}
  \item{sst}{
destination of .nc file for the SST data from CoRDAT
}
 \item{ssta}{
destination of .nc file for the SSTA data from CoRDAT
}
  \item{tsa}{
destination of .nc file for the TSA anomaly data from CoRDAT
}
  \item{lon}{
longitude (in decimal degrees)
}
  \item{lat}{
latitude (in decimal degrees)
}
  \item{method}{
See the argument method in extract function
}
  \item{buffer}{
See the argument buffer in extract function; default is set to 5000
}
  \item{destfile}{
a destination file to save the file, default (NULL)
}
}

\value{
This function returns to a data frame with following variables:
  \item{date}{time of observation (class: date; format: YYYY-MM-DD)}
  \item{SST}{90th percentile of Sea Surface Temperature in degree C (class: numeric)}
  \item{min_SST}{Minimum Sea Surface Temperature in degree C (class: numeric)}
  \item{max_SST}{Maximum Sea Surface Temperature in degree C (class: numeric)}
  \item{SSTA}{Sea Surface Temperature Anomaly in degree C (class: numeric)}
  \item{TSA}{Thermal Stress Anomaly in degee C (class: numeric)}
  \item{DHW}{Degree Heating Weeks in degree C - weeks (class: numeric)}
  \item{source}{databases, e.g. Coral Reef Watch and Coral Reef Temperature Anomaly Database (class: character)}
  }
\references{

NOAA CRW. Coral Reef Watch Satellite Monitoring and Modeled Outlooks. https://coralreefwatch.noaa.gov/satellite/index.php

NOAA CRW. Regional Virtual Stations Product Description. https://coralreefwatch.noaa.gov/vs/map.php

Liu, G., A.E. Strong, W.J. Skirving and L.F. Arzayus (2006). Overview of NOAA Coral Reef Watch Program's Near-Real-Time Satellite Global Coral Bleaching Monitoring Activities. Proceedings of the 10th International Coral Reef Symposium, Okinawa: 1783-1793.

National Center for Atmospheric Research Staff (Eds). Last modified 20 Aug 2013. "The Climate Data Guide: CoRTAD: Coral Reef Temperature Anomaly Database (SST)." Retrieved from https://climatedataguide.ucar.edu/climate-data/cortad-coral-reef-temperature-anomaly-database-sst.

Link to CoRTAD database: ftp://ftp.nodc.noaa.gov/pub/data.nodc/cortad/Version5/
}

\examples{
# GET TIME SERIES DATA FROM CRW

url <- "https://coralreefwatch.noaa.gov/vs/data/bahrain.txt"

## download file without saving
coralwatch <- get.crw(url)

## download and save file
coralwatch <- get.crw(url, destfile = "crw.csv")

head(coralwatch)

# GET TIME SERIES DATA FROM CoRTAD

## .nc files (BIG files!) can be downloaded from ftp://ftp.nodc.noaa.gov/pub/data.nodc/cortad/Version5/
sst <- "cortadv5_WeeklySST.nc"
ssta <- "cortadv5_SSTA.nc"
tsa <- "cortadv5_TSA.nc"
lon <-  50.725
lat <- 26.125

## load file w/out saving
cortad <- get.cortad(sst,ssta,tsa, lon,lat, method = "bilinear", buffer = 5000)

## load and save file
cortad <- get.cortad(sst,ssta,tsa, lon,lat, method = "bilinear", buffer = 5000, destfile = "cortad.csv")

head(cordat)


# GET AND MERGE TIME SERIES DATA FROM CRW and CoRTAD

## load file w/out saving
df <- get.cortad.crw(url, sst, ssta, tsa, method = "bilinear", buffer = 5000)

## load and save file
df <- get.cortad.crw(url, sst, ssta, tsa, method = "bilinear", buffer = 5000, destfile = "./Data/combined.csv")

head(df)
