get.cortad <- function(sst,ssta,tsa,lon, lat, method = "bilinear", buffer = 5000, destfile = NULL) {
  if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
  if(!require(imputeTS)){install.packages("imputeTS"); library(imputeTS)}
  if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
  if(!require(raster)){install.packages("raster"); library(raster)}

  extract.timeseries <- function(nc, varname, coor, method = "bilinear", buffer = 5000, unit = "Kelvin"){
    nc.brick <- brick(nc, varname = varname)
    x <- raster:: extract(nc.brick,coor, method = method, buffer = buffer, fun = function(x) quantile(x, probs = 0.9))
    x <- dplyr:: add_rownames(as.data.frame(t(x)))
    x$rowname <- gsub("X","",x$rowname)
    x$rowname <- as.Date(x$rowname, format = "%Y.%m.%d")
    colnames(x) <- c("date", paste0("p_",varname))


    y <- raster:: extract(nc.brick, coor, method = method, buffer = buffer, fun = min)
    y <- dplyr:: add_rownames(as.data.frame(t(y)))
    y$rowname <- gsub("X","",y$rowname)
    y$rowname <- as.Date(y$rowname, format = "%Y.%m.%d")
    colnames(y) <- c("date", paste0("min_",varname))

    z <- raster:: extract(nc.brick, coor, method = method, buffer = buffer, fun = max)
    z <- dplyr:: add_rownames(as.data.frame(t(z)))
    z$rowname <- gsub("X","",z$rowname)
    z$rowname <- as.Date(z$rowname, format = "%Y.%m.%d")
    colnames(z) <- c("date", paste0("max_",varname))

    df <- list(x, y, z) %>% reduce(full_join, by = "date")
    temp.unit <- unit
    df[2:4]  <- ifelse(temp.unit == "Celsius",  df[2:4] - 273.15, df[2:4])
    df
  }

  sst.ts <- extract.timeseries(nc = sst, varname = "WeeklySST", coor = cbind(lon,lat),method, buffer, unit = "Celsius")
  ssta.ts <- extract.timeseries(nc = ssta, varname = "SSTA", coor = cbind(lon,lat),method, buffer)
  tsa.ts <- extract.timeseries(nc = tsa, varname = "TSA", coor = cbind(lon,lat),method, buffer)

  compute.dhw <- function(x, y){
    # insert event id, week and year column
    dhw.ts <- as.data.frame(x)
    dhw.ts$event_id <- seq(1,length(dhw.ts$x))
    colnames(dhw.ts)[1] <- c("date")
    dhw.ts$week <- week(dhw.ts$date)
    dhw.ts$year <- year(dhw.ts$date)
    dhw.ts$TSA <- y
    dhw.ts$tsa_dhw <- ifelse(dhw.ts$TSA >= 1 , dhw.ts$TSA , 0)
    dhw <- function(event_id = 12){sum(dhw.ts$tsa_dhw[dhw.ts$event_id >= event_id-11 & dhw.ts$event_id <= event_id])}
    for (i in unique(dhw.ts$event_id)){
      x <- dhw(i)
      y <- c(event_id = i, DHW = x)
      assign(paste0("DHW_",i), as.data.frame(t(y)))
    }
    dhw_list <- mget(ls(pattern = "DHW_"))
    dhw.climatology <- bind_rows(dhw_list)
    dhw.summary <- merge(dhw.ts, dhw.climatology, by = "event_id")
    dhw.summary$DHW
  }



  #compute DHW based on TSA
  dhw.ts <- as.data.frame(tsa.ts$date)
  dhw.ts$p_DHW <- compute.dhw(tsa.ts$date,tsa.ts$p_TSA)
  dhw.ts$min_DHW <- compute.dhw(tsa.ts$date, tsa.ts$min_TSA)
  dhw.ts$max_DHW <- compute.dhw(tsa.ts$date, tsa.ts$max_TSA)
  colnames(dhw.ts)[1] <- c("date")

  #clear cortad for merging with coral watch data sets
  df.cortad <- list(sst.ts, ssta.ts, tsa.ts, dhw.ts) %>% reduce(full_join, by = "date")
  cortad <- dplyr:: select(df.cortad, date,
                           SST = p_WeeklySST,
                           min_SST = min_WeeklySST,
                           max_SST = max_WeeklySST,
                           SSTA = p_SSTA,
                           TSA = p_TSA,
                           DHW = p_DHW)
  cortad$source <- "Coral Reef Temperature Anomaly Database (CoRTAD)"
  invisible(if(!is.null(destfile)){write.csv(cortad, destfile)})
  cortad
}
