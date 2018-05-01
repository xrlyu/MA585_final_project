library(ggplot2)
library(forecast)
library(tseries)
library(TSA)

# denoted in millions of dollars, adjusted for inflation
box_office <- readRDS("box_office(long).rds")

train <- ts(box_office[1:96], start = c(2008,1), end = c(2015,12), frequency = 12)
test <- ts(box_office[97:123], start = c(2016,1), end = c(2018,3), frequency = 12)

box_office2 <- as.data.frame(box_office)
box_office2$Time <- seq(as.Date("2008/01/01"), as.Date("2018/03/01"), "month")
names(box_office2)[1] <- "Box"

train_p <- box_office2[1:96,]

# train_p_l <- train_p
# 
# train_p_l$Box <- log(train_p_l$Box)
# 
# ggplot(train_p_l, aes(Time, Box)) + geom_line() +
#   scale_x_date(date_labels = "%b-%Y", date_breaks = "3 months") + 
#   xlab("") + ylab("log(Gross Box Office)") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
#         plot.title = element_text(size = 14, hjust = 0.5)) + 
#   ggtitle("Log of Box Office Data")

z <- decompose(train, type = "additive")
plot(z)

diff <- ts(diff(diff(train, lag = 12)), start = c(2008,1), end = c(2015,12), frequency = 12)
plot(diff)

# ARIMA model
par(mfrow=c(1,2))
acf(diff, lag.max = 48)
pacf(diff, lag.max = 48)

fit_arima_1 <- Arima(train, order = c(1,2,0)) # AR(1) model
fit_arima_1

fit_arima_2 <- Arima(train, order = c(0,2,1)) # MA(1) model
fit_arima_2

fit_arima_3 <- Arima(train, order = c(1,2,1)) # ARMA(1,1) model
fit_arima_3
tsdiag(fit_arima_3)

dev.off()

fit_armasub <- armasubsets(y = diff, nar = 7, nma = 15, y.name = "fit")
plot(fit_armasub)

fit_arima_4 <- Arima(train, order = c(0,2,12), fixed = c(NA, rep(0,10), NA)) # MA(12) model
fit_arima_4 

# SARIMA model

fit_sarima_1 <- Arima(train, order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))
fit_sarima_1  # MA(1)

fit_sarima_2 <- Arima(train, order = c(1,1,1), seasonal = list(order = c(0,1,1), period = 12))
fit_sarima_2  # ARMA(1,1)

fit_sarima_3 <- Arima(train, order = c(1,1,0), seasonal = list(order = c(0,1,1), period = 12))
fit_sarima_3  # AR(1)

# forecast

### ARIMA model
ARIMAfcast <- forecast(fit_arima_2, h = 27)
ARIMAerr <- test - ARIMAfcast$mean
ARIMAmae <- mean(abs(ARIMAerr))
ARIMArmse <- sqrt(mean(ARIMAerr^2))
ARIMAmapez <- mean(abs(ARIMAerr*100)/test)

### SARIMA model
SARIMAfcast_1 <- forecast(fit_sarima_1, h = 27)
SARIMAerr_1 <- test - SARIMAfcast_1$mean
SARIMAmae_1 <- mean(abs(SARIMAerr_1))
SARIMArmse_1 <- sqrt(mean(SARIMAerr_1^2))
SARIMAmapez_1 <- mean(abs(SARIMAerr_1*100)/test)

SARIMAfcast_2 <- forecast(fit_sarima_2, h = 27)
SARIMAerr_2 <- test - SARIMAfcast_2$mean
SARIMAmae_2 <- mean(abs(SARIMAerr_2))
SARIMArmse_2 <- sqrt(mean(SARIMAerr_2^2))
SARIMAmapez_2 <- mean(abs(SARIMAerr_2*100)/test)

### Holt-Winters method
fit_h <- HoltWinters(train, gamma = TRUE)
HWfcast <- forecast(fit_h, h = 27)
HWerr <- test - HWfcast$mean
HWmae <- mean(abs(HWerr))
HWrmse <- sqrt(mean(HWerr^2))
HWmapez <- mean(abs((HWerr*100)/test))

### final model
fit <- Arima(box_office, model = fit_sarima_2)
forecast(fit, h = 9)

plot(forecast(fit_sarima_2, h=27))
lines(test, col = 'red')
SARIMAerr_2

fit_2 <- Arima(box_office, model = fit_sarima_2)
forecast(fit_2, h = 9)
