if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}

clean.timeseries <- function(x,y, start_date, end_date, by = "week"){
  ts.df <-as.data.frame(list(date = x, estimate = y))
  nd <- as.data.frame(seq.Date(start_date, end_date, by))
  colnames(nd) <- "date"
  ts.df1 <- full_join(ts.df,nd,by="date")
  ts.df1$date <- round_date(ts.df1$date, by)
  ts.df1 <- ts.df1[order(ts.df1$date),]

  ts.df2 <- ts.df1 %>%
    group_by(date) %>%
    summarise(estimate = mean(estimate))
  ts.df2$estimate <- na.interpolation(ts.df2$estimate)
  ts.df2
}
