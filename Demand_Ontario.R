

setwd("P:/Courses/MMA 860 - Alexander Scott Acquisition and Data Management/Project/TS/Ontario_Demand")
library(readxl)
# install.packages("tseries")
library(tseries)
library(forecast)
library(sqldf)
library(dplyr)
library(ggplot2)
library(lubridate)

options(scipen=999)
file <- read.csv("P:\\Courses\\MMA 860 - Alexander Scott Acquisition and Data Management\\Project\\Final Datasets\\demand_datasets\\Final_Demand_Ontario.csv")
pre <- read_excel("P:\\Courses\\MMA 860 - Alexander Scott Acquisition and Data Management\\Project\\Final Datasets\\demand_datasets\\Pred2020.xlsx") #dataset with null values for 2020 to be predicted
str(pre)
pre$Date <- as.Date(pre$Date)

str(file)

file$Date1 <- as.Date(file$Date1, "%d-%m-%Y")
str(file)
file$month <- months(file$Date1)
file$year <- year(file$Date1)

file$month_year <- paste0(months(file$Date1), "-", year(file$Date1))
#paste is working as concatenate here


file1 <- file %>% group_by(month_year) %>% summarise(Market.Demand=sum(Market.Demand), 
                                                          Ontario.Demand=sum(Ontario.Demand))

file1$Date <- dmy(paste0("01-",file1$month_year))

file1 <- dplyr::arrange(file1, Date)

#Divide into Test and train

Train <- filter(file1, Date <= "2019-12-01" )
Test <-  filter(file1, Date > "2019-12-01" )
Test1 <- rbind(Test, pre)

#Analysing the components of time series data
Ontario_ts <- ts(Train$Ontario.Demand,start=c(2008,1),end = c(2019,12),frequency=12)

#Plots for dataset and TS
plot(decompose(Ontario_ts))
autoplot(decompose(Ontario_ts))
monthplot(Ontario_ts, labels=1:12, xlab="Months")
seasonplot(Ontario_ts, col=c("Red", "Blue"), xlabs= " ", year.labels = T,  cex=0.9, main="Season Plot for Ontario Electricity Generation")
plot(cycle(Ontario_ts))
ggmonthplot(Ontario_ts)
ggseasonplot(Ontario_ts)


#Forecasting the numbers in TS for 2020,2021
Ontario_ts_reg <- auto.arima(Ontario_ts) #Order of seasonal differencing=1


#Check for ACF, PACF as autocorrelation in the ts forecast
tsdisplay(Ontario_ts_reg$residuals, main = " Autocorrelation Plots: Ontario")



plot(forecast(Ontario_ts_reg, 24))
df_predicted <- data.frame(forecast(Ontario_ts_reg, 24))

accuracy <- cbind(Test1,df_predicted)
#t <- df_predicted %>% select(c('VALUE','Point.Forecast'))
accuracy$diff <- (accuracy$Point.Forecast-accuracy$Ontario.Demand)/accuracy$Ontario.Demand
library(plotly)

fig <- plotly::plot_ly(accuracy,  y = ~Ontario.Demand, x= ~Date, name = 'Actual Value', 
                       type = 'scatter', mode = 'lines', 
                       line = list(color = 'rgb(15, 19, 167)', width = 2))  %>% 
  add_trace(y = ~Point.Forecast, name = 'Forecasted', mode = 'lines', 
            line = list(color = 'rgb(100, 39, 67)', width = 2, dash = 'dash')) %>%
  layout(title = "Forecasted Electricity Demand (ONTARIO 2020-2021)",
         xaxis = list(title = "Months"),
         yaxis = list (title = "Electricity"))

fig

accuracy(Ontario_ts_reg)
mean(accuracy$diff, na.rm = T)

write.csv(accuracy, "Forecast_with_confidence.csv")
write.csv(Train, "Train_data.csv")

