extract.timeseries.cordat <- function(nc, varname, coor, fun = mean, method = "bilinear", buffer = 5000){
  if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
  if(!require(imputeTS)){install.packages("imputeTS"); library(imputeTS)}
  if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
  if(!require(raster)){install.packages("raster"); library(raster)}
  nc.brick <- brick(nc, varname = varname)
  x <- raster:: extract(nc.brick,coor, method = method, buffer = buffer, fun = fun)
  x <- dplyr:: add_rownames(as.data.frame(t(x)))
  x$rowname <- gsub("X","",x$rowname)
  x$rowname <- as.Date(x$rowname, format = "%Y.%m.%d")
  colnames(x) <- c("date", paste0(varname))
  x
}
