if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}

get.crw <- function(url,destfile = NULL){
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
  invisible(if(!is.null(destfile)){write.csv(df2, destfile)})
  df2
}


