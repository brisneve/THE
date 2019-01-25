get.cortad.crw <- function(url, sst, ssta, tsa, method = "bilinear", buffer = 5000, destfile = NULL){
  if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
  if(!require(imputeTS)){install.packages("imputeTS"); library(imputeTS)}
  if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
  if(!require(raster)){install.packages("raster"); library(raster)}

  #download CRW timeseries
  crw <- get.crw(url)

  #extract midpoints of the virtual station
  get.midpoints <- function(url){
    url1 <-  readLines(url)
    mid.lon <- as.numeric(url1[5])
    mid.lat <- as.numeric(url1[8])
    mid.points <- list(mid.lon = mid.lon, mid.lat = mid.lat)
    mid.points
  }
  lon <- as.numeric(get.midpoints(url)[1])
  lat <- as.numeric(get.midpoints(url)[2])

  crtd <- get.cortad(sst,ssta,tsa, lon, lat, method, buffer)

  df <- bind_rows(crtd,crw)
  df$SST <- na.interpolation(df$SST)
  df$min_SST <- na.interpolation(df$min_SST)
  df$max_SST <- na.interpolation(df$max_SST)
  df$SSTA <- na.interpolation(df$SSTA)
  df$TSA <- na.interpolation(df$TSA)
  df$DHW <- na.interpolation(df$DHW)
  df$TSA <- ifelse(df$TSA > 0 , df$TSA, 0)
  arrange(df, date)
  invisible(if(!is.null(destfile)){write.csv(df, destfile)})
  df
}
