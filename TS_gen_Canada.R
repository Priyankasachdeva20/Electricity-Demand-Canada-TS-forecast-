

rm(list=ls())

setwd("P:\\Courses\\MMA 860 - Alexander Scott Acquisition and Data Management\\Project\\TS")
library(readxl)
# install.packages("tseries")
library(tseries)
library(forecast)
library(data.table)
library(dplyr)
library(ggplot2)


options(scipen=999)
file <- read.csv("P:\\Courses\\MMA 860 - Alexander Scott Acquisition and Data Management\\Project\\Final Datasets\\25100016Power_by_components_Canada.csv")
pre <- read_excel("Pred2020.xlsx") #dataset with null values for 2020 to be predicted
Canada_Gen <- filter(file, file$GEO=="Canada" & file$Electric.power..components=="Total generation")

#converting ref date from char to date format as TS requires, add date as date is missing (def- 01)
Canada_Gen$REF_DATE <- as.Date(paste0(Canada_Gen$ï..REF_DATE,"-01"),"%Y-%m-%d")
pre$REF_DATE <- as.Date(paste0(pre$REF_DATE,"-01"),"%Y-%m-%d")
#paste is working as concatenate here

#Divide into Test and train

Train <- filter(Canada_Gen, REF_DATE < "2019-01-01" )
Test <-  filter(Canada_Gen, REF_DATE >= "2019-01-01" )
Test1 <- rbind(subset(Test, select=c("REF_DATE", "VALUE")), pre)


#Analysing the components of time series data
Canada_Gen_ts <- ts(Train$VALUE,start=c(2008,1),end = c(2018,12),frequency=12)

#Plots for dataset and TS
plot(decompose(Canada_Gen_ts))
autoplot(decompose(Canada_Gen_ts))
monthplot(Canada_Gen_ts, labels=1:12, xlab="Months")
seasonplot(Canada_Gen_ts, col=c("Red", "Blue"), xlabs= " ", year.labels = T,  cex=0.9, main="Season Plot for Canada Electricity Generation")
plot(cycle(Canada_Gen_ts))
ggmonthplot(Canada_Gen_ts)
ggseasonplot(Canada_Gen_ts)


tsdisplay(Canada_Gen_ts_reg$residuals, main = " Autocorrelation Plots: Canada")
acf(Canada_Gen_ts_reg$residuals)
Pacf(Canada_Gen_ts_reg$residuals)

#Forecasting the numbers in TS for 2019
Canada_Gen_ts_reg <- auto.arima(Canada_Gen_ts, trace=T) #Order of seasonal differencing=1

plot(forecast(Canada_Gen_ts_reg, 24))
df_predicted <- data.frame(forecast(Canada_Gen_ts_reg, 24))

accuracy <- cbind(Test1,df_predicted)
#t <- df_predicted %>% select(c('VALUE','Point.Forecast'))
accuracy$diff <- (accuracy$Point.Forecast-accuracy$VALUE)/accuracy$VALUE
mean(accuracy$diff, na.rm = T)
accuracy(Canada_Gen_ts_reg)

fig <- plotly::plot_ly(accuracy,  y = ~VALUE, x= ~REF_DATE, name = 'Actual Value', 
                       type = 'scatter', mode = 'lines', 
                       line = list(color = 'rgb(15, 19, 167)', width = 2))  %>% 
                       add_trace(y = ~Point.Forecast, name = 'Forecasted', mode = 'lines', 
                       line = list(color = 'rgb(100, 39, 67)', width = 2, dash = 'dash')) %>%
                       layout(title = "Forecasted Electricity generation (CANADA 2019-2020)",
                       xaxis = list(title = "Months"),
                       yaxis = list (title = "Electricity"))

fig

write.csv(accuracy, "Forecast_with_confidence.csv")


