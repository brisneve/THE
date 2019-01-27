plot.crw <- function(url, start = NULL, end = NULL, destfile = NULL, width = 8, height = 4){
  if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
  if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
  if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}
  if(!require(ggthemes)){install.packages("ggthemes"); library(ggthemes)}

  df1 <- read.table(url, skip=21, header = TRUE)
  df1$date <- as.Date(with(df1, paste0(YYYY,"-",MM,"-",DD)))
  df2 <- dplyr:: select(df1, date,
                        SST = `SST.90th_HS`,
                        min_SST = SST_MIN,
                        max_SST = SST_MAX,
                        SSTA = `SSTA.90th_HS`,
                        TSA = `X90th_HS.0`,
                        DHW = `DHW_from_90th_HS.1`)
  df2$source <- "Coral Reef Watch"
  txt <- readLines(url)

  df2$month <- month(df2$date)
  df2$site <- txt[2]
  df2$lon <-as.numeric(txt[5])
  df2$lat <-as.numeric(txt[8])


  df3 <- as.data.frame(as.numeric(unlist(str_split(txt[14], " "))))
  df3 <- rownames_to_column(df3)
  colnames(df3) <- c("month", "MMM")

  df4 <- merge(df2,df3,by="month")

  df5 <- df4 %>%
    filter(date >= ifelse(is.null(start), first(df4$date), start) & date <= ifelse(is.null(start), last(df4$date), end))


  sst.offset <- max(df4$SST)/max(df4$DHW)
  sec.axis.offset <- max(df4$DHW)/max(df4$SST)

  p <- ggplot(df5)+
    geom_ribbon(aes(x=date, ymin = min_SST, ymax = max_SST), fill = "gray", alpha = 0.5)+
    geom_line(aes(x=date, y=SST), color = "navyblue")+
    geom_point(aes(x=date, y=MMM), color = "black", size = 0.1)+
    geom_hline(yintercept = max(df4$MMM) + 1, color = "navyblue", lty =3)+
    geom_hline(yintercept = max(df4$MMM), color ="navyblue", lty =2, size = 0.25)+
    geom_line(aes(x=date, y=DHW*sst.offset), color = "darkred")+
    geom_hline(yintercept = 4*sst.offset, color = "darkred", lty = 2, size = 0.25)+
    geom_hline(yintercept = 8*sst.offset, color = "darkred", lty =3)+
    xlab(expression("Date"))+
    ylab(expression(SST~(degree~C)))+
    scale_y_continuous(sec.axis = sec_axis( ~.*sec.axis.offset, name = expression(DHW~(degree~C~"-"~weeks))))+
    theme_light()+
    theme(axis.ticks.y.right = element_line(color = "darkred"),
          axis.text.y.right = element_text(color = "darkred"),
          axis.title.y.right = element_text(color = "darkred"))+
    theme(axis.ticks.y.left = element_line(color = "navyblue"),
          axis.text.y.left = element_text(color = "navyblue"),
          axis.title.y.left = element_text(color = "navyblue"))+
    theme(axis.line.y.right = element_line(color = "darkred"),
          axis.line.y.left = element_line(color = "navyblue"))

  print.plot <- function(destfile, width,height){
    pdf(destfile, width, height)
    print(p)
    graphics.off()
  }
  invisible(if(!is.null(destfile)){print.plot(destfile, width, height)})

  print(p)
}
