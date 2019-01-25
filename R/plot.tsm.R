plot.tsm <- function (x) {

  df2 <- x

  if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}
  if(!require(ggthemes)){install.packages("ggthemes"); library(ggthemes)}

  p1 <- ggplot(df2, aes(x =date, y = SST))+
    geom_line(color="darkgray")+
    geom_hline(yintercept = df2$min_climatology, lty = 2, color = "blue")+
    geom_hline(yintercept = df2$max_climatology, lty = 2, color = "red")+
    geom_hline(yintercept = df2$max_climatology + 1, lty = 1, color = "red")+
    theme_linedraw()+
    ylab(expression(SST~(degree~C)))+
    scale_y_continuous(sec.axis = sec_axis( ~.*1, name = expression(SST~(degree~C))))+
    geom_smooth(method = "lm")+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  #theme(text = element_text(size=20))

  df3 <- df2 %>%
    mutate(year = lubridate:: year(date)) %>%
    group_by(year) %>%
    summarise(max_dhw = max(DHW)) %>%
    filter(max_dhw > 4) %>%
    mutate(DHW_label = ifelse(max_dhw > 8 , "coral mortality", "coral bleaching"))

  df4 <- df2 %>%
    mutate(year = lubridate:: year(date)) %>%
    filter(year %in% df3$year) %>%
    group_by(year) %>%
    summarise(date = date[max(DHW)])

  df5 <- merge(df3,df4, by="year")


  p2 <- ggplot(df2, aes(date, DHW)) +
    geom_line(color = "gray", alpha = 1, size = 1) +
    geom_line(aes(date, TSA*4), color = "black") +
    xlab("Date")+
    ylab(expression(DHW~(degree~C~"-"~weeks)))+
    scale_y_continuous(sec.axis = sec_axis( ~.*0.25 , name = expression(TSA~(degree~C))))+
    geom_hline(yintercept = 4, lty = 2, color = "gray")+
    geom_hline(yintercept = 8, lty = 2, color = "gray")+
    geom_point(data = df5, aes(x=date,y=max_dhw,color = DHW_label), size = 4)+
    geom_text(data = df5, aes(x=date,y=max_dhw, label=year, color = DHW_label),hjust=0, vjust=0)+
    theme_linedraw()+
    theme(legend.position = "none",
          legend.background = element_rect(fill ="transparent"))+
    labs(color = "Legend")+
    scale_color_manual(values = c("blue", "red"))
  #theme(text = element_text(size=20))

  g <- cowplot:: plot_grid(p1,p2,
                           labels=c('A.', 'B.'), nrow = 2)
  #label_size = 20, nrow = 2)


  print(g)
}
