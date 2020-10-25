library(forecast)


# Individual timeseries objects (Credit card and debit card PoS volume)
cc_pos_vol_apr04_oct16 <- ts(apr04_oct16_payments$ccd_pos_vol, frequency = 12, start = c(2004, 4))
plot(cc_pos_vol_apr04_oct16)
cc_pos_vol_nov16_feb20 <- ts(nov16_feb20_payments$ccd_pos_vol, frequency = 12, start = c(2016, 11))
plot(cc_pos_vol_nov16_feb20)

dc_pos_vol_apr04_oct16 <- ts(apr04_oct16_payments$dcd_pos_vol, frequency = 12, start = c(2004,4))
plot(dc_pos_vol_apr04_oct16)
dc_pos_vol_nov16_feb20 <- ts(nov16_feb20_payments$dcd_pos_vol, frequency = 12, start = c(2016, 11))
plot(dc_pos_vol_nov16_feb20)


# Debit card PoS forecast:
## Dc pos volume forecast using 2008-2016 data
dc_pos_vol_apr08_oct16 <- window(dc_pos_vol_apr04_oct16, start = c(2008, 4))

dc_vol_arima <- auto.arima(dc_pos_vol_apr08_oct16, lambda = "auto")
dc_vol_arima_forecast <- forecast(dc_vol_arima, h = 40)

### Actual dc PoS volume after demonetization
dc_pos_vol_nov16_oct19 <- window(dc_pos_vol_nov16_feb20, end = c(2019, 10))
plot(dc_pos_vol_nov16_oct19)

dc_vol_arima_forecast %>% plot(main = "Forecast of debit card use at PoS from ARIMA model", ylim = c(0, 4500),
                               xlab = ("Year"),
                               ylab = ("Debit card usage volume at PoS (in lakhs)"))
lines(dc_pos_vol_nov16_oct19, col = "red", lwd = c(2))
abline(v=2016+9/12, col="6", lwd = c(2))
legend("topleft",lty = c(1,1,1), y.intersp = 0.5, x.intersp = 0.4, lwd = c(2,2,2), cex = 0.8, c("Forecast mean", "Actual Value", "Demonetization"), col = c("blue", "red", "6"))


# Credit card PoS volume forecast:
## Cc PoS volume forecast using 2004-2016 data
plot(cc_pos_vol_apr04_oct16)

cc_vol_arima <- auto.arima(cc_pos_vol_apr04_oct16, lambda = "auto")
cc_vol_arima_forecast <- forecast(cc_vol_arima, h = 40)

### Actual cc PoS volume after demonetization
cc_pos_vol_nov16_oct19 <- window(cc_pos_vol_nov16_feb20, end = c(2019, 10))
plot(cc_vol08_arima_forecast)

cc_vol_arima_forecast %>% plot(main = "Forecast of credit card use at PoS from ARIMA model",
                               xlab = ("Year"),
                               ylab = ("Credit card volume at PoS (in lakhs)"))
lines(cc_pos_vol_nov16_oct19, col = "red", lwd = c(2))
abline(v=2016+9/12, col="6", lwd = c(2))
legend("topleft",lty = c(1,1,1), lwd = c(2,2,2), y.intersp = 0.5, x.intersp = 0.4, cex = 0.8, c("Forecast mean", "Actual Value", "Demonetization"), col = c("blue", "red", "6"))

## Cc pos volume forecast using 2008-2016 data
cc_pos_vol_apr08_oct16 <- window(cc_pos_vol_apr04_oct16, start = c(2008, 4))
plot(cc_pos_vol_apr08_oct16)

cc_vol08_arima <- auto.arima(cc_pos_vol_apr08_oct16, lambda="auto")
cc_vol08_arima_forecast <- forecast(cc_vol08_arima, h = 40)

cc_vol08_arima_forecast %>% plot(main = "Forecast of credit card use at PoS from ARIMA model",
                               xlab = ("Year"),
                               ylab = ("Credit card volume at PoS (in lakhs)"))
lines(cc_pos_vol_nov16_oct19, col = "red", lwd = c(2))
abline(v=2016+9/12,col="6", lwd = c(2))
legend("topleft",lty = c(1,1,1), y.intersp = 0.5, x.intersp = 0.4, lwd = c(2,2,2), cex = 0.8, c("Forecast mean", "Actual Value", "Demonetization"), col = c("blue", "red", "6"))


# Individual timeseries objects (Currency with the public)
currency_with_public_apr08_oct16 <- window(currency_with_public_apr91_jul20, start = c(2008, 4), end = c(2016, 10))
plot(currency_with_public_apr08_oct16)
currency_with_public_apr91_oct16 <- window(currency_with_public_apr91_jul20, end = c(2016, 10))
plot(currency_with_public_apr91_oct16)
currency_with_public_nov16_feb20 <- window(currency_with_public_apr91_jul20, start = c(2016, 11), end = c(2020, 2))
plot(currency_with_public_nov16_feb20)


# Cash with public in the economy forecast:
## Cash with public forecast using 2008-2016 data
currency_with_public_arima <- auto.arima(currency_with_public_apr08_oct16, lambda = "auto")
currency_with_public_arima_forecast <- forecast(currency_with_public_arima, h = 40)

currency_with_public_arima_forecast %>% plot(main = "Forecast of currency with the public from ARIMA model",
                               xlab = ("Year"),
                               ylab = ("Currency with the public (in Crore Rupees)"))
lines(currency_with_public_nov16_feb20, col = "red", lwd = c(2))
abline(v=2016+9/12, col="6", lwd = c(2))
legend("topleft",lty = c(1,1,1), lwd = c(2,2,2), y.intersp = 0.5, x.intersp = 0.4, cex = 0.8, c("Forecast mean", "Actual Value", "Demonetization"), col = c("blue", "red", "6"))

## cash with public forecast using 1991-2016 data
currency_with_public_arima1 <- auto.arima(currency_with_public_apr91_oct16, lambda = "auto")
currency_with_public_arima1
currency_with_public_arima1_forecast <- forecast(currency_with_public_arima1, h = 40)

currency_with_public_arima1_forecast %>% plot(main = "Forecast of currency with the public from ARIMA model",
                                             xlab = ("Year"),
                                             ylab = ("Currency with the public (in Crore Rupees)"))
lines(currency_with_public_nov16_feb20, col = "red", lwd = c(2))
abline(v=2016+9/12, col="6", lwd = c(2))
legend("topleft",lty = c(1,1,1), lwd = c(2,2,2), y.intersp = 0.5, x.intersp = 0.4, cex = 0.8, c("Forecast mean", "Actual Value", "Demonetization"), col = c("blue", "red", "6"))


# Individual timeseries objects (Average ATM withdrawal)
avrg_atm_withdrawl_apr11_oct19 <- ts(dc_avrg_atm_withdrawl$avrg_withdrawl, frequency = 12, start = c(2011, 4))
plot(avrg_atm_withdrawl_apr11_oct19)

avrg_atm_withdrawl_apr11_oct16 <- window(avrg_atm_withdrawl_apr11_oct19, end = c(2016, 10))
avrg_atm_withdrawl_nov16_oct19 <- window(avrg_atm_withdrawl_apr11_oct19, start = c(2016, 11))


# ATM average withdrawal amount forecast (using 2011-2016 data):
avrg_atm_withdrawl_arima <- auto.arima(avrg_atm_withdrawl_apr11_oct16, lambda = "auto")
avrg_atm_withdrawl_arima_forecast <- forecast(avrg_atm_withdrawl_arima, h = 40)

avrg_atm_withdrawl_arima_forecast %>% plot(main = "Forecast of average ATM withdrawal from ARIMA model",
                                          xlab = ("Year"),
                                          ylab = ("Average amount withdrawn from ATM (in Rupees)"),
                                          ylim = c(1000, 4200))
lines(avrg_atm_withdrawl_nov16_oct19, col = "red", lwd = c(2))
abline(v=2016+9/12, col="6", lwd = c(2))
legend("topleft",lty = c(1,1,1), lwd = c(2,2,2), y.intersp = 0.5, x.intersp = 0.4, cex = 0.8, c("Forecast mean", "Actual Value", "Demonetization"), col = c("blue", "red", "6"))

