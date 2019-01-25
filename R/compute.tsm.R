compute.tsm <- function(x,y){
  #create dataframe
  df <-as.data.frame(list(date = x, estimate = y))

  #compute climatology
  df1 <- df %>%
    group_by(month = month(date)) %>%
    mutate(monthly_mean = mean(estimate))
  df1$max_climatology = max(df1$monthly_mean)
  df1$min_climatology = min(df1$monthly_mean)
  df1$mean_climatology = mean(df1$monthly_mean)

  #compute sea suface temperature anomaly
  df1$SSTA = df1$estimate - df1$mean_climatology

  #compute thermal stress anomaly
  df1$TSA = df1$estimate - df1$max_climatology
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
  df1$DHW = compute.dhw(df1$date,df1$TSA)

  colnames(df1)[2] <- "SST"
  df2 <- dplyr:: select(df1, -month)
  df2
}
