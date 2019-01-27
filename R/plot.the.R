plot.the <- function(df, start = NULL, end = NULL, destfile = NULL, width = 8, height = 4){

  if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
  if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}
  if(!require(ggthemes)){install.packages("ggthemes"); library(ggthemes)}


  #compute climatology
  df1 <- na.omit(df) %>%
    group_by(week=week(date)) %>%
    mutate(climatology = mean(SST)) %>%
    ungroup() %>%
    filter(date >= ifelse(is.null(start), first(date), start) & date <= ifelse(is.null(start), last(date), end))


  sst.offset <- max(df1$SST)/max(df1$DHW)
  sec.axis.offset <- max(df1$DHW)/max(df1$SST)

  p <- ggplot(df1)+
    geom_line(aes(x=date, y=SST), color = "navyblue")+
    geom_point(aes(x=date, y=climatology), color = "black", size = 0.1)+
    geom_hline(yintercept = max(df1$climatology) + 1, color = "navyblue", lty =3)+
    geom_hline(yintercept = max(df1$climatology), color ="navyblue", lty =2, size = 0.25)+
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
