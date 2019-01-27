compute.the <- function(x,y,z ="week"){

  if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
  if(!require(imputeTS)){install.packages("imputeTS"); library(imputeTS)}
  if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}

  #create dataframe
  df <-as.data.frame(list(date = x, estimate = na.interpolation(y)))

  #compute climatology
  df1 <- df %>%
    group_by(grp = eval(parse(text=paste0(z,"(date)")))) %>%
    mutate(climatology = mean(estimate)) %>%
    ungroup()


  #compute sea suface temperature anomaly
  df1$SSTA = df1$estimate - df1$climatology

  #compute thermal stress anomaly
  df1$TSA = df1$estimate - max(df1$climatology)
  df1$TSA = ifelse(df1$TSA > 0 , df1$TSA , 0)

  #compute degree heating weeks
  compute.dhw <- function(x, y){
    # insert event id, week and year column
    dhw.ts <- as.data.frame(x)
    dhw.ts$event_id <- seq(1,length(dhw.ts$x))
    colnames(dhw.ts)[1] <- c("date")
    dhw.ts$week <- week(dhw.ts$date)
    dhw.ts$year <- year(dhw.ts$date)
    dhw.ts$TSA <- y
    dhw.ts$tsa_dhw <- ifelse(dhw.ts$TSA >= 1, dhw.ts$TSA , 0)

    nz <- ifelse(z == "month", 89,
                 ifelse(z == "week", 11, "NA"))

    dhw <- function(event_id){sum(dhw.ts$tsa_dhw[dhw.ts$event_id >= event_id-nz & dhw.ts$event_id <= event_id])}
    for (i in unique(dhw.ts$event_id)){
      x <- dhw(i)
      y <- c(event_id = i, DHW = x)
      assign(paste0("DHW_",i), as.data.frame(t(y)))
    }
    dhw_list <- mget(ls(pattern = "DHW_"))
    dhw.climatology <- bind_rows(dhw_list)
    dhw.summary <- merge(dhw.ts, dhw.climatology, by = "event_id")
    (dhw.summary$DHW)
  }
  df1$DHW = compute.dhw(df1$date,df1$TSA)

  colnames(df1)[2] <- "SST"
  df2 <- dplyr:: select(df1, date, SST, climatology,SSTA,TSA,DHW)
}

