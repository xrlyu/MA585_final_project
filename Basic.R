library(boxoffice)
library(dplyr)
library(plyr)
library(lubridate)

dates <- seq(from = as.Date("2013/01/01"), to = as.Date("2018/03/31"), by = "day")

data <- boxoffice(dates)

data2 <- data %>% 
  dplyr::select(gross, date) %>% 
  filter(date <= as.Date("2018-03-01"))

data2$my <- floor_date(data2$date, "month")
series <- ddply(data2, "my", summarise, box_office = mean(gross))
series$year <- year(series$my)

inflation <- read.csv("inflation rate.csv")
names(inflation) <- c('year', 'inflation')
inflation$inflation <- inflation$inflation /100
inflation$year <- year(inflation$year)

full <- left_join(series, inflation) %>% replace_na(list(inflation = list(1.0522092)))
full$inflation <- as.numeric(full$inflation)
full$gross <- full$box_office * full$inflation

box_office <- full %>% dplyr::select(my, gross)

# make box_office as time series
box_office <- ts(box_office$gross, start = c(2013,1), end = c(2018,3), frequency = 12)

# use box_office for time series analysis
saveRDS(box_office, file = "box_office.rds")