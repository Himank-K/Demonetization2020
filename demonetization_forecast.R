library(forecast)
library(ggplot2)

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


# Debit card ATM volume:
dc_atm_vol_apr04_oct19 <- ts(digital_payments$dcd_atm_vol, start = c(2004, 4), frequency = 12)
dc_atm_vol_apr11_oct19 <- window(dc_atm_vol_apr04_oct19, start = c(2011, 4))

plot(dc_atm_vol_apr11_oct19,
     main = "Debit card ATM volume",
     xlab = ("Year"),
     ylab = ("Debit card ATM volume (in lakhs)"),
)
abline(v=2016+9/12, col="6", lwd = c(2))
legend("topleft",lty = c(1), lwd = c(2), y.intersp = 0.5, x.intersp = 0.4, cex = 0.8, c("Demonetization"), col = c("6"))


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


# ATM average withdrawal amount (using 2011-2016 data):
plot(avrg_atm_withdrawl_apr11_oct19,
     main = "Average ATM withdrawal",
     xlab = ("Year"),
     ylab = ("Average amount withdrawn from ATM (in rupees)"),
)
abline(v=2016+9/12, col="6", lwd = c(2))
legend("topleft",lty = c(1), lwd = c(2), y.intersp = 0.5, x.intersp = 0.4, cex = 0.8, c("Demonetization"), col = c("6"))


# ATM average withdrawal amount forecast (using 2011-2016 data):
avrg_atm_withdrawl_arima <- auto.arima(avrg_atm_withdrawl_apr11_oct16, lambda = "auto")
avrg_atm_withdrawl_arima_forecast <- forecast(avrg_atm_withdrawl_arima, h = 36)

avrg_atm_withdrawl_arima_forecast %>% plot(main = "Forecast of average ATM withdrawal from ARIMA model",
                                           xlab = ("Year"),
                                           ylab = ("Average amount withdrawn from ATM (in Rupees)"),
                                           ylim = c(1000, 4200))
lines(avrg_atm_withdrawl_nov16_oct19, col = "red", lwd = c(2))
abline(v=2016+9/12, col="6", lwd = c(2))
legend("topleft",lty = c(1,1,1), lwd = c(2,2,2), y.intersp = 0.5, x.intersp = 0.4, cex = 0.8, c("Forecast mean", "Actual Value", "Demonetization"), col = c("blue", "red", "6"))



# Individual timeseries object (No. of PoS)
no_of_pos_ts <- ts(no_of_pos$no_of_pos, frequency = 12, start = c(2004, 4))
plot(no_of_pos_ts)
no_of_pos_apr11_oct19 <- window(no_of_pos_ts, start = c(2011, 04))
no_of_pos_apr11_oct16 <- window(no_of_pos_apr11_oct19, end = c(2016, 10))
no_of_pos_nov16_oct19 <- window(no_of_pos_apr11_oct19, start = c(2016, 11))

plot(no_of_pos_apr11_oct19,
     main = "Number of PoS",
     xlab = ("Year"),
     ylab = ("No. of PoS"),
)
abline(v=2016+9/12, col="6", lwd = c(2))
legend("topleft",lty = c(1), lwd = c(2), y.intersp = 0.5, x.intersp = 0.4, cex = 0.8, c("Demonetization"), col = c("6"))


# Individual timeseries objects (PPI and UPI)
ppi_vol_ts <- ts(digital_payments$ppi_vol, start = c(2004, 4), frequency = 12)
upi_vol_ts <- ts(digital_payments$upi_vol, start = c(2004, 4), frequency = 12)

ppi_vol_apr11_oct19 <- window(ppi_vol_ts, start = c(2011, 4))
upi_vol_apr16_oct19 <- window(upi_vol_ts, start = c(2016, 4))

plot(ppi_vol_apr11_oct19,
     main = "PPI volume",
     xlab = ("Year"),
     ylab = ("PPI volume (in lakhs)"),
)
abline(v=2016+9/12, col="6", lwd = c(2))
legend("topleft",lty = c(1), lwd = c(2), y.intersp = 0.5, x.intersp = 0.4, cex = 0.8, c("Demonetization"), col = c("6"))

plot(upi_vol_apr16_oct19,
     main = "UPI volume",
     xlab = ("Year"),
     ylab = ("UPI volume (in lakhs)"),
     xaxt = "n"
)
axis(side = 1, at = 2016:2020)
abline(v=2016+9/12, col="6", lwd = c(2))
legend("topleft",lty = c(1), lwd = c(2), y.intersp = 0.5, x.intersp = 0.4, cex = 0.8, c("Demonetization"), col = c("6"))


# Plotting digital payments
## Volume
digital_payments_vol_apr04_oct19 <- data.frame(
  Time = digital_payments$month_year,
  upi = digital_payments$upi_vol,
  ppi = digital_payments$ppi_vol,
  dc_pos = digital_payments$dcd_pos_vol,
  cd_pos = digital_payments$ccd_pos_vol
)

digital_payments_vol_apr04_oct19_m <- melt(digital_payments_vol_apr04_oct19, "Time")
ggplot()+geom_bar(aes(x = Time, y = value, fill = variable), data = digital_payments_vol_apr04_oct19_m, stat = "identity")+
  theme_classic()+
  labs(x = "Year", y = "Volume in lakhs", title = "Non cash payment modes transaction\n")+
  theme(legend.position = c(0.08, 0.89),
        legend.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.title = element_text(hjust = 0.5, size = 18)
  )+
  scale_fill_discrete(labels = c("UPI", "PPI", "Debit card PoS", "Credit card PoS"))

## Value
digital_payments_value_apr04_oct19 <- data.frame(
  Time = digital_payments$month_year,
  upi = digital_payments$upi_val,
  ppi = digital_payments$ppi_value,
  dc_pos = digital_payments$dcd_pos_value,
  cd_pos = digital_payments$ccd_pos_value
)

digital_payments_value_apr04_oct19_m <- melt(digital_payments_value_apr04_oct19, "Time")
ggplot()+geom_bar(aes(x = Time, y = value, fill = variable), data = digital_payments_value_apr04_oct19_m, stat = "identity")+
  theme_classic()+
  labs(x = "Year", y = "Value in crore rupees", title = "Non cash payment modes value\n")+
  theme(legend.position = c(0.08, 0.89),
        legend.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.title = element_text(hjust = 0.5, size = 18)
  )+
  scale_fill_discrete(labels = c("UPI", "PPI", "Debit card PoS", "Credit card PoS"))

