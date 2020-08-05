
library(readxl)
# install.packages("tseries")
# install.packages("sqldf")
library(tseries)
library(forecast)
library(data.table)
library(dplyr)
library(ggplot2)
library(sqldf)

options(scipen=999)
file <- read.csv("P:\\Courses\\MMA 860 - Alexander Scott Acquisition and Data Management\\Project\\Final Datasets\\25100016Power_by_components_Canada.csv")
pre <- read_excel("P:\\Courses\\MMA 860 - Alexander Scott Acquisition and Data Management\\Project\\TS\\Pred2020.xlsx") #dataset with null values for 2020 to be predicted
file1 <- filter(file, file$REF_DATE>="2016-01-01")


#converting ref date from char to date format as TS requires, add date as date is missing (def- 01)
file$REF_DATE <- as.Date(paste0(file$ï..REF_DATE,"-01"),"%Y-%m-%d")
file1$REF_DATE <- as.Date(paste0(file1$ï..REF_DATE,"-01"),"%Y-%m-%d")
pre$REF_DATE <- as.Date(paste0(pre$REF_DATE,"-01"),"%Y-%m-%d")

#paste is working as concatenate here

x <- sqldf( "select DISTINCT(GEO) from file" )
x
NFL_Gen <- filter(file, file$GEO=="Newfoundland and Labrador" & file$Electric.power..components=="Total generation")
NWT_Gen <- filter(file, file$GEO=="Northwest Territories" & file$Electric.power..components=="Total generation")
PEI_Gen <- filter(file1, file1$GEO=="Prince Edward Island" & file1$Electric.power..components=="Total generation")
NS_Gen <- filter(file, file$GEO=="Nova Scotia" & file$Electric.power..components=="Total generation")
NB_Gen <- filter(file, file$GEO=="New Brunswick" & file$Electric.power..components=="Total generation")
QB_Gen <- filter(file, file$GEO=="Quebec" & file$Electric.power..components=="Total generation")
ON_Gen <- filter(file, file$GEO=="Ontario" & file$Electric.power..components=="Total generation")
MA_Gen <- filter(file, file$GEO=="Manitoba" & file$Electric.power..components=="Total generation")
SA_Gen <- filter(file, file$GEO=="Saskatchewan" & file$Electric.power..components=="Total generation")
Al_Gen <- filter(file, file$GEO=="Alberta" & file$Electric.power..components=="Total generation")
BC_Gen <- filter(file, file$GEO=="British Columbia" & file$Electric.power..components=="Total generation")
YU_Gen <- filter(file, file$GEO=="Yukon" & file$Electric.power..components=="Total generation")
NU_Gen <- filter(file, file$GEO=="Nunavut" & file$Electric.power..components=="Total generation")



#Divide into Test and train

Train1 <- filter(NFL_Gen, REF_DATE < "2019-01-01" )
Test1 <-  filter(NFL_Gen, REF_DATE >= "2019-01-01" )
Test11 <- rbind(subset(Test1, select=c("REF_DATE", "VALUE")), pre)

Train2 <- filter(NWT_Gen, REF_DATE < "2019-01-01" )
Test2 <-  filter(NWT_Gen, REF_DATE >= "2019-01-01" )
Test22 <- rbind(subset(Test2, select=c("REF_DATE", "VALUE")), pre)

Train3 <- filter(PEI_Gen, REF_DATE < "2019-01-01" )
Test3 <-  filter(PEI_Gen, REF_DATE >= "2019-01-01" )
Test33 <- rbind(subset(Test3, select=c("REF_DATE", "VALUE")), pre)

Train4 <- filter(NS_Gen, REF_DATE < "2019-01-01" )
Test4 <-  filter(NS_Gen, REF_DATE >= "2019-01-01" )
Test44 <- rbind(subset(Test4, select=c("REF_DATE", "VALUE")), pre)

Train5 <- filter(NB_Gen, REF_DATE < "2019-01-01" )
Test5 <-  filter(NB_Gen, REF_DATE >= "2019-01-01" )
Test55 <- rbind(subset(Test5, select=c("REF_DATE", "VALUE")), pre)

Train6 <- filter(QB_Gen, REF_DATE < "2019-01-01" )
Test6 <-  filter(QB_Gen, REF_DATE >= "2019-01-01" )
Test66 <- rbind(subset(Test6, select=c("REF_DATE", "VALUE")), pre)

Train7 <- filter(ON_Gen, REF_DATE < "2019-01-01" )
Test7 <-  filter(ON_Gen, REF_DATE >= "2019-01-01" )
Test77 <- rbind(subset(Test7, select=c("REF_DATE", "VALUE")), pre)

Train8 <- filter(MA_Gen, REF_DATE < "2019-01-01" )
Test8 <-  filter(MA_Gen, REF_DATE >= "2019-01-01" )
Test88 <- rbind(subset(Test8, select=c("REF_DATE", "VALUE")), pre)

Train9 <- filter(SA_Gen, REF_DATE < "2019-01-01" )
Test9 <-  filter(SA_Gen, REF_DATE >= "2019-01-01" )
Test99 <- rbind(subset(Test9, select=c("REF_DATE", "VALUE")), pre)

Train10 <- filter(Al_Gen, REF_DATE < "2019-01-01" )
Test100 <-  filter(Al_Gen, REF_DATE >= "2019-01-01" )
Test101 <- rbind(subset(Test100, select=c("REF_DATE", "VALUE")), pre)

Train11 <- filter(BC_Gen, REF_DATE < "2019-01-01" )
Test110 <-  filter(BC_Gen, REF_DATE >= "2019-01-01" )
Test111 <- rbind(subset(Test110, select=c("REF_DATE", "VALUE")), pre)

Train12 <- filter(YU_Gen, REF_DATE < "2019-01-01" )
Test120 <-  filter(YU_Gen, REF_DATE >= "2019-01-01" )
Test121 <- rbind(subset(Test120, select=c("REF_DATE", "VALUE")), pre)

Train13 <- filter(NU_Gen, REF_DATE < "2019-01-01" )
Test130 <-  filter(NU_Gen, REF_DATE >= "2019-01-01" )
Test131 <- rbind(subset(Test130, select=c("REF_DATE", "VALUE")), pre)

#Analysing the components of time series data
NFL_Gen_ts <- ts(Train1$VALUE,start=c(2008,1),end = c(2018,12),frequency=12)
NWT_Gen_ts <- ts(Train2$VALUE,start=c(2008,1),end = c(2018,12),frequency=12)
PEI_Gen_ts <- ts(Train3$VALUE,start=c(2008,1),end = c(2018,12),frequency=12)
NS_Gen_ts <- ts(Train4$VALUE,start=c(2008,1),end = c(2018,12),frequency=12)
NB_Gen_ts <- ts(Train5$VALUE,start=c(2008,1),end = c(2018,12),frequency=12)
QB_Gen_ts <- ts(Train6$VALUE,start=c(2008,1),end = c(2018,12),frequency=12)
ON_Gen_ts <- ts(Train7$VALUE,start=c(2008,1),end = c(2018,12),frequency=12)
MA_Gen_ts <- ts(Train8$VALUE,start=c(2008,1),end = c(2018,12),frequency=12)
SA_Gen_ts <- ts(Train9$VALUE,start=c(2008,1),end = c(2018,12),frequency=12)
Al_Gen_ts <- ts(Train10$VALUE,start=c(2008,1),end = c(2018,12),frequency=12)
BC_Gen_ts <- ts(Train11$VALUE,start=c(2008,1),end = c(2018,12),frequency=12)
YU_Gen_ts <- ts(Train12$VALUE,start=c(2008,1),end = c(2018,12),frequency=12)
NU_Gen_ts <- ts(Train13$VALUE,start=c(2008,1),end = c(2018,12),frequency=12)

#--------------------FORECAST FOR NFL------------------------------------------

#Plots for dataset and TS
plot(decompose(NFL_Gen_ts))
autoplot(decompose(NFL_Gen_ts))
monthplot(NFL_Gen_ts, labels=1:12, xlab="Months")
seasonplot(NFL_Gen_ts, col=c("Red", "Blue"), xlabs= " ", year.labels = T,  cex=0.9, main="Season Plot for Canada Electricity Generation")
plot(cycle(NFL_Gen_ts))
ggmonthplot(NFL_Gen_ts)
ggseasonplot(NFL_Gen_ts)


#Forecasting the numbers in TS for 2019
NFL_Gen_ts_reg <- auto.arima(NFL_Gen_ts) #Order of seasonal differencing=1

plot(forecast(NFL_Gen_ts_reg, 24))
df_predicted1 <- data.frame(forecast(NFL_Gen_ts_reg, 24))

accuracy1 <- cbind(Test11,df_predicted1)
#t <- df_predicted %>% select(c('VALUE','Point.Forecast'))
accuracy1$diff <- (accuracy1$Point.Forecast-accuracy1$VALUE)/accuracy1$VALUE

library(plotly)
fig1 <- plotly::plot_ly(accuracy1,  y = ~VALUE, x= ~REF_DATE, name = 'Actual Value', 
                       type = 'scatter', mode = 'lines', 
                       line = list(color = 'rgb(15, 19, 167)', width = 2))  %>% 
  add_trace(y = ~Point.Forecast, name = 'Forecasted', mode = 'lines', 
            line = list(color = 'rgb(100, 39, 67)', width = 2, dash = 'dash')) %>%
  layout(title = "Forecasted Electricity generation (NFL 2019-2020)",
         xaxis = list(title = "Months"),
         yaxis = list (title = "Electricity"))

fig1

write.csv(accuracy1, "Forecast_with_confidence_NFL.csv")

#-----------------------------------------------------------------------------------------------------

#--------------------FORECAST FOR NWT------------------------------------------

#Plots for dataset and TS
plot(decompose(NWT_Gen_ts))
autoplot(decompose(NWT_Gen_ts))
monthplot(NWT_Gen_ts, labels=1:12, xlab="Months")
seasonplot(NWT_Gen_ts, col=c("Red", "Blue"), xlabs= " ", year.labels = T,  cex=0.9, main="Season Plot for Canada Electricity Generation")
plot(cycle(NWT_Gen_ts))
ggmonthplot(NWT_Gen_ts)
ggseasonplot(NWT_Gen_ts)


#Forecasting the numbers in TS for 2019
NWT_Gen_ts_reg <- auto.arima(NWT_Gen_ts) #Order of seasonal differencing=1

plot(forecast(NWT_Gen_ts_reg, 24))
df_predicted2 <- data.frame(forecast(NWT_Gen_ts_reg, 24))

accuracy2 <- cbind(Test22,df_predicted2)
#t <- df_predicted %>% select(c('VALUE','Point.Forecast'))
accuracy2$diff <- (accuracy2$Point.Forecast-accuracy2$VALUE)/accuracy2$VALUE

library(plotly)
fig2 <- plotly::plot_ly(accuracy2,  y = ~VALUE, x= ~REF_DATE, name = 'Actual Value', 
                        type = 'scatter', mode = 'lines', 
                        line = list(color = 'rgb(15, 19, 167)', width = 2))  %>% 
  add_trace(y = ~Point.Forecast, name = 'Forecasted', mode = 'lines', 
            line = list(color = 'rgb(100, 39, 67)', width = 2, dash = 'dash')) %>%
  layout(title = "Forecasted Electricity generation (NWT 2019-2020)",
         xaxis = list(title = "Months"),
         yaxis = list (title = "Electricity"))

fig2

write.csv(accuracy2, "Forecast_with_confidence_NWT.csv")

#-----------------------------------------------------------------------------------------------------

#--------------------FORECAST FOR PEI------------------------------------------

#Plots for dataset and TS
plot(decompose(PEI_Gen_ts))
autoplot(decompose(PEI_Gen_ts))
monthplot(PEI_Gen_ts, labels=1:12, xlab="Months")
seasonplot(PEI_Gen_ts, col=c("Red", "Blue"), xlabs= " ", year.labels = T,  cex=0.9, main="Season Plot for Canada Electricity Generation")
plot(cycle(PEI_Gen_ts))
ggmonthplot(PEI_Gen_ts)
ggseasonplot(PEI_Gen_ts)


#Forecasting the numbers in TS for 2019
PEI_Gen_ts_reg <- auto.arima(PEI_Gen_ts, allowdrift = T) #Order of seasonal differencing=1

plot(forecast(PEI_Gen_ts_reg, 24))


#---------checks
kpss.test(PEI_Gen_ts)
PP.test(PEI_Gen_ts)
adf.test(PEI_Gen_ts)

install.packages("fUnitRoots")
library(fUnitRoots)
adfTest(PEI_Gen_ts,type = c("ct")) #fUNITroot Package
unitrootTest(PEI_Gen_ts)


df_predicted3 <- data.frame(forecast(PEI_Gen_ts_reg, 24))

accuracy3 <- cbind(Test33,df_predicted3)
#t <- df_predicted %>% select(c('VALUE','Point.Forecast'))
accuracy3$diff <- (accuracy3$Point.Forecast-accuracy3$VALUE)/accuracy3$VALUE

library(plotly)
fig3 <- plotly::plot_ly(accuracy3,  y = ~VALUE, x= ~REF_DATE, name = 'Actual Value', 
                        type = 'scatter', mode = 'lines', 
                        line = list(color = 'rgb(15, 19, 167)', width = 2))  %>% 
  add_trace(y = ~Point.Forecast, name = 'Forecasted', mode = 'lines', 
            line = list(color = 'rgb(100, 39, 67)', width = 2, dash = 'dash')) %>%
  layout(title = "Forecasted Electricity generation (PEI 2019-2020)",
         xaxis = list(title = "Months"),
         yaxis = list (title = "Electricity"))

fig3

write.csv(accuracy3, "Forecast_with_confidence_PEI_v3.csv")

#-----------------------------------------------------------------------------------------------------

#--------------------FORECAST FOR NS------------------------------------------

#Plots for dataset and TS
plot(decompose(NS_Gen_ts))
autoplot(decompose(NS_Gen_ts))
monthplot(NS_Gen_ts, labels=1:12, xlab="Months")
seasonplot(NS_Gen_ts, col=c("Red", "Blue"), xlabs= " ", year.labels = T,  cex=0.9, main="Season Plot for Canada Electricity Generation")
plot(cycle(NS_Gen_ts))
ggmonthplot(NS_Gen_ts)
ggseasonplot(NS_Gen_ts)


#Forecasting the numbers in TS for 2019
NS_Gen_ts_reg <- auto.arima(NS_Gen_ts) #Order of seasonal differencing=1

plot(forecast(NS_Gen_ts_reg, 24))
df_predicted4 <- data.frame(forecast(NS_Gen_ts_reg, 24))

accuracy4 <- cbind(Test44,df_predicted4)
#t <- df_predicted %>% select(c('VALUE','Point.Forecast'))
accuracy4$diff <- (accuracy4$Point.Forecast-accuracy4$VALUE)/accuracy4$VALUE

library(plotly)
fig4 <- plotly::plot_ly(accuracy4,  y = ~VALUE, x= ~REF_DATE, name = 'Actual Value', 
                        type = 'scatter', mode = 'lines', 
                        line = list(color = 'rgb(15, 19, 167)', width = 2))  %>% 
  add_trace(y = ~Point.Forecast, name = 'Forecasted', mode = 'lines', 
            line = list(color = 'rgb(100, 39, 67)', width = 2, dash = 'dash')) %>%
  layout(title = "Forecasted Electricity generation (NS 2019-2020)",
         xaxis = list(title = "Months"),
         yaxis = list (title = "Electricity"))

fig4

write.csv(accuracy4, "Forecast_with_confidence_NS.csv")

#-----------------------------------------------------------------------------------------------------

#--------------------FORECAST FOR NB------------------------------------------

#Plots for dataset and TS
plot(decompose(NB_Gen_ts))
autoplot(decompose(NB_Gen_ts))
monthplot(NB_Gen_ts, labels=1:12, xlab="Months")
seasonplot(NB_Gen_ts, col=c("Red", "Blue"), xlabs= " ", year.labels = T,  cex=0.9, main="Season Plot for Canada Electricity Generation")
plot(cycle(NB_Gen_ts))
ggmonthplot(NB_Gen_ts)
ggseasonplot(NB_Gen_ts)


#Forecasting the numbers in TS for 2019
NB_Gen_ts_reg <- auto.arima(NB_Gen_ts) #Order of seasonal differencing=1

plot(forecast(NB_Gen_ts_reg, 24))
df_predicted5 <- data.frame(forecast(NB_Gen_ts_reg, 24))

accuracy5 <- cbind(Test55,df_predicted5)
#t <- df_predicted %>% select(c('VALUE','Point.Forecast'))
accuracy5$diff <- (accuracy5$Point.Forecast-accuracy5$VALUE)/accuracy5$VALUE

library(plotly)
fig5 <- plotly::plot_ly(accuracy5,  y = ~VALUE, x= ~REF_DATE, name = 'Actual Value', 
                        type = 'scatter', mode = 'lines', 
                        line = list(color = 'rgb(15, 19, 167)', width = 2))  %>% 
  add_trace(y = ~Point.Forecast, name = 'Forecasted', mode = 'lines', 
            line = list(color = 'rgb(100, 39, 67)', width = 2, dash = 'dash')) %>%
  layout(title = "Forecasted Electricity generation (NB 2019-2020)",
         xaxis = list(title = "Months"),
         yaxis = list (title = "Electricity"))

fig5

write.csv(accuracy5, "Forecast_with_confidence_NB.csv")

#-----------------------------------------------------------------------------------------------------

#--------------------FORECAST FOR QB------------------------------------------

#Plots for dataset and TS
plot(decompose(QB_Gen_ts))
autoplot(decompose(QB_Gen_ts))
monthplot(QB_Gen_ts, labels=1:12, xlab="Months")
seasonplot(QB_Gen_ts, col=c("Red", "Blue"), xlabs= " ", year.labels = T,  cex=0.9, main="Season Plot for Canada Electricity Generation")
plot(cycle(QB_Gen_ts))
ggmonthplot(QB_Gen_ts)
ggseasonplot(QB_Gen_ts)


#Forecasting the numbers in TS for 2019
QB_Gen_ts_reg <- auto.arima(QB_Gen_ts) #Order of seasonal differencing=1

plot(forecast(QB_Gen_ts_reg, 24))
df_predicted6 <- data.frame(forecast(QB_Gen_ts_reg, 24))

accuracy6 <- cbind(Test66,df_predicted6)
#t <- df_predicted %>% select(c('VALUE','Point.Forecast'))
accuracy6$diff <- (accuracy6$Point.Forecast-accuracy6$VALUE)/accuracy6$VALUE

library(plotly)
fig6 <- plotly::plot_ly(accuracy6,  y = ~VALUE, x= ~REF_DATE, name = 'Actual Value', 
                        type = 'scatter', mode = 'lines', 
                        line = list(color = 'rgb(15, 19, 167)', width = 2))  %>% 
  add_trace(y = ~Point.Forecast, name = 'Forecasted', mode = 'lines', 
            line = list(color = 'rgb(100, 39, 67)', width = 2, dash = 'dash')) %>%
  layout(title = "Forecasted Electricity generation (QB 2019-2020)",
         xaxis = list(title = "Months"),
         yaxis = list (title = "Electricity"))

fig6

write.csv(accuracy6, "Forecast_with_confidence_QB.csv")

#-----------------------------------------------------------------------------------------------------


#--------------------FORECAST FOR ON------------------------------------------

#Plots for dataset and TS
plot(decompose(ON_Gen_ts))
autoplot(decompose(ON_Gen_ts))
monthplot(ON_Gen_ts, labels=1:12, xlab="Months")
seasonplot(ON_Gen_ts, col=c("Red", "Blue"), xlabs= " ", year.labels = T,  cex=0.9, main="Season Plot for Canada Electricity Generation")
plot(cycle(ON_Gen_ts))
ggmonthplot(ON_Gen_ts)
ggseasonplot(ON_Gen_ts)


#Forecasting the numbers in TS for 2019
ON_Gen_ts_reg <- auto.arima(ON_Gen_ts) #Order of seasonal differencing=1

plot(forecast(ON_Gen_ts_reg, 24))
df_predicted7 <- data.frame(forecast(ON_Gen_ts_reg, 24))

accuracy7 <- cbind(Test77,df_predicted7)
#t <- df_predicted %>% select(c('VALUE','Point.Forecast'))
accuracy7$diff <- (accuracy7$Point.Forecast-accuracy7$VALUE)/accuracy7$VALUE

library(plotly)
fig7 <- plotly::plot_ly(accuracy7,  y = ~VALUE, x= ~REF_DATE, name = 'Actual Value', 
                        type = 'scatter', mode = 'lines', 
                        line = list(color = 'rgb(15, 19, 167)', width = 2))  %>% 
  add_trace(y = ~Point.Forecast, name = 'Forecasted', mode = 'lines', 
            line = list(color = 'rgb(100, 39, 67)', width = 2, dash = 'dash')) %>%
  layout(title = "Forecasted Electricity generation (ON 2019-2020)",
         xaxis = list(title = "Months"),
         yaxis = list (title = "Electricity"))

fig7

write.csv(accuracy7, "Forecast_with_confidence_ON.csv")
#-----------------------------------------------------------------------------------------------------


#--------------------FORECAST FOR MA------------------------------------------

#Plots for dataset and TS
plot(decompose(MA_Gen_ts))
autoplot(decompose(MA_Gen_ts))
monthplot(MA_Gen_ts, labels=1:12, xlab="Months")
seasonplot(MA_Gen_ts, col=c("Red", "Blue"), xlabs= " ", year.labels = T,  cex=0.9, main="Season Plot for Canada Electricity Generation")
plot(cycle(MA_Gen_ts))
ggmonthplot(MA_Gen_ts)
ggseasonplot(MA_Gen_ts)


#Forecasting the numbers in TS for 2019
MA_Gen_ts_reg <- auto.arima(MA_Gen_ts) #Order of seasonal differencing=1

plot(forecast(MA_Gen_ts_reg, 24))
df_predicted8 <- data.frame(forecast(MA_Gen_ts_reg, 24))

accuracy8 <- cbind(Test88,df_predicted8)
#t <- df_predicted %>% select(c('VALUE','Point.Forecast'))
accuracy8$diff <- (accuracy8$Point.Forecast-accuracy8$VALUE)/accuracy8$VALUE

library(plotly)
fig8 <- plotly::plot_ly(accuracy8,  y = ~VALUE, x= ~REF_DATE, name = 'Actual Value', 
                        type = 'scatter', mode = 'lines', 
                        line = list(color = 'rgb(15, 19, 167)', width = 2))  %>% 
  add_trace(y = ~Point.Forecast, name = 'Forecasted', mode = 'lines', 
            line = list(color = 'rgb(100, 39, 67)', width = 2, dash = 'dash')) %>%
  layout(title = "Forecasted Electricity generation (MA 2019-2020)",
         xaxis = list(title = "Months"),
         yaxis = list (title = "Electricity"))

fig8

write.csv(accuracy8, "Forecast_with_confidence_MA.csv")
#-----------------------------------------------------------------------------------------------------


#--------------------FORECAST FOR SA------------------------------------------

#Plots for dataset and TS
plot(decompose(SA_Gen_ts))
autoplot(decompose(SA_Gen_ts))
monthplot(SA_Gen_ts, labels=1:12, xlab="Months")
seasonplot(SA_Gen_ts, col=c("Red", "Blue"), xlabs= " ", year.labels = T,  cex=0.9, main="Season Plot for Canada Electricity Generation")
plot(cycle(SA_Gen_ts))
ggmonthplot(SA_Gen_ts)
ggseasonplot(SA_Gen_ts)


#Forecasting the numbers in TS for 2019
SA_Gen_ts_reg <- auto.arima(SA_Gen_ts) #Order of seasonal differencing=1

plot(forecast(SA_Gen_ts_reg, 24))
df_predicted9 <- data.frame(forecast(SA_Gen_ts_reg, 24))

accuracy9 <- cbind(Test99,df_predicted9)
#t <- df_predicted %>% select(c('VALUE','Point.Forecast'))
accuracy9$diff <- (accuracy9$Point.Forecast-accuracy9$VALUE)/accuracy9$VALUE

library(plotly)
fig9 <- plotly::plot_ly(accuracy9,  y = ~VALUE, x= ~REF_DATE, name = 'Actual Value', 
                        type = 'scatter', mode = 'lines', 
                        line = list(color = 'rgb(15, 19, 167)', width = 2))  %>% 
  add_trace(y = ~Point.Forecast, name = 'Forecasted', mode = 'lines', 
            line = list(color = 'rgb(100, 39, 67)', width = 2, dash = 'dash')) %>%
  layout(title = "Forecasted Electricity generation (SA 2019-2020)",
         xaxis = list(title = "Months"),
         yaxis = list (title = "Electricity"))

fig9

write.csv(accuracy9, "Forecast_with_confidence_SA.csv")
#-----------------------------------------------------------------------------------------------------


#--------------------FORECAST FOR Al------------------------------------------

#Plots for dataset and TS
plot(decompose(Al_Gen_ts))
autoplot(decompose(Al_Gen_ts))
monthplot(Al_Gen_ts, labels=1:12, xlab="Months")
seasonplot(Al_Gen_ts, col=c("Red", "Blue"), xlabs= " ", year.labels = T,  cex=0.9, main="Season Plot for Canada Electricity Generation")
plot(cycle(Al_Gen_ts))
ggmonthplot(Al_Gen_ts)
ggseasonplot(Al_Gen_ts)


#Forecasting the numbers in TS for 2019
Al_Gen_ts_reg <- auto.arima(Al_Gen_ts) #Order of seasonal differencing=1

plot(forecast(Al_Gen_ts_reg, 24))
df_predicted10 <- data.frame(forecast(Al_Gen_ts_reg, 24))

accuracy10 <- cbind(Test101,df_predicted10)
#t <- df_predicted %>% select(c('VALUE','Point.Forecast'))
accuracy10$diff <- (accuracy10$Point.Forecast-accuracy10$VALUE)/accuracy10$VALUE

library(plotly)
fig10 <- plotly::plot_ly(accuracy10,  y = ~VALUE, x= ~REF_DATE, name = 'Actual Value', 
                        type = 'scatter', mode = 'lines', 
                        line = list(color = 'rgb(15, 19, 167)', width = 2))  %>% 
  add_trace(y = ~Point.Forecast, name = 'Forecasted', mode = 'lines', 
            line = list(color = 'rgb(100, 39, 67)', width = 2, dash = 'dash')) %>%
  layout(title = "Forecasted Electricity generation (AL 2019-2020)",
         xaxis = list(title = "Months"),
         yaxis = list (title = "Electricity"))

fig10

write.csv(accuracy10, "Forecast_with_confidence_Al.csv")
#-----------------------------------------------------------------------------------------------------

#--------------------FORECAST FOR BC------------------------------------------

#Plots for dataset and TS
plot(decompose(BC_Gen_ts))
autoplot(decompose(BC_Gen_ts))
monthplot(BC_Gen_ts, labels=1:12, xlab="Months")
seasonplot(BC_Gen_ts, col=c("Red", "Blue"), xlabs= " ", year.labels = T,  cex=0.9, main="Season Plot for Canada Electricity Generation")
plot(cycle(BC_Gen_ts))
ggmonthplot(BC_Gen_ts)
ggseasonplot(BC_Gen_ts)


#Forecasting the numbers in TS for 2019
BC_Gen_ts_reg <- auto.arima(BC_Gen_ts) #Order of seasonal differencing=1

plot(forecast(BC_Gen_ts_reg, 24))
df_predicted11 <- data.frame(forecast(BC_Gen_ts_reg, 24))

accuracy11 <- cbind(Test111,df_predicted11)
#t <- df_predicted %>% select(c('VALUE','Point.Forecast'))
accuracy11$diff <- (accuracy11$Point.Forecast-accuracy11$VALUE)/accuracy11$VALUE

library(plotly)
fig11 <- plotly::plot_ly(accuracy11,  y = ~VALUE, x= ~REF_DATE, name = 'Actual Value', 
                        type = 'scatter', mode = 'lines', 
                        line = list(color = 'rgb(15, 19, 167)', width = 2))  %>% 
  add_trace(y = ~Point.Forecast, name = 'Forecasted', mode = 'lines', 
            line = list(color = 'rgb(100, 39, 67)', width = 2, dash = 'dash')) %>%
  layout(title = "Forecasted Electricity generation (BC 2019-2020)",
         xaxis = list(title = "Months"),
         yaxis = list (title = "Electricity"))

fig11

write.csv(accuracy11, "Forecast_with_confidence_BC.csv")
#-----------------------------------------------------------------------------------------------------

#--------------------FORECAST FOR YU------------------------------------------

#Plots for dataset and TS
plot(decompose(YU_Gen_ts))
autoplot(decompose(YU_Gen_ts))
monthplot(YU_Gen_ts, labels=1:12, xlab="Months")
seasonplot(YU_Gen_ts, col=c("Red", "Blue"), xlabs= " ", year.labels = T,  cex=0.9, main="Season Plot for Canada Electricity Generation")
plot(cycle(YU_Gen_ts))
ggmonthplot(YU_Gen_ts)
ggseasonplot(YU_Gen_ts)


#Forecasting the numbers in TS for 2019
YU_Gen_ts_reg <- auto.arima(YU_Gen_ts) #Order of seasonal differencing=1

plot(forecast(YU_Gen_ts_reg, 24))
df_predicted12 <- data.frame(forecast(YU_Gen_ts_reg, 24))

accuracy12 <- cbind(Test121,df_predicted12)
#t <- df_predicted %>% select(c('VALUE','Point.Forecast'))
accuracy12$diff <- (accuracy12$Point.Forecast-accuracy12$VALUE)/accuracy12$VALUE

library(plotly)
fig12 <- plotly::plot_ly(accuracy12,  y = ~VALUE, x= ~REF_DATE, name = 'Actual Value', 
                        type = 'scatter', mode = 'lines', 
                        line = list(color = 'rgb(15, 19, 167)', width = 2))  %>% 
  add_trace(y = ~Point.Forecast, name = 'Forecasted', mode = 'lines', 
            line = list(color = 'rgb(100, 39, 67)', width = 2, dash = 'dash')) %>%
  layout(title = "Forecasted Electricity generation (YU 2019-2020)",
         xaxis = list(title = "Months"),
         yaxis = list (title = "Electricity"))

fig12

write.csv(accuracy12, "Forecast_with_confidence_YU.csv")
#-----------------------------------------------------------------------------------------------------

#--------------------FORECAST FOR NU------------------------------------------

#Plots for dataset and TS
plot(decompose(NU_Gen_ts))
autoplot(decompose(NU_Gen_ts))
monthplot(NU_Gen_ts, labels=1:12, xlab="Months")
seasonplot(NU_Gen_ts, col=c("Red", "Blue"), xlabs= " ", year.labels = T,  cex=0.9, main="Season Plot for Canada Electricity Generation")
plot(cycle(NU_Gen_ts))
ggmonthplot(NU_Gen_ts)
ggseasonplot(NU_Gen_ts)


#Forecasting the numbers in TS for 2019
NU_Gen_ts_reg <- auto.arima(NU_Gen_ts) #Order of seasonal differencing=1

plot(forecast(NU_Gen_ts_reg, 24))
df_predicted13 <- data.frame(forecast(NU_Gen_ts_reg, 24))

accuracy13 <- cbind(Test131,df_predicted13)
#t <- df_predicted %>% select(c('VALUE','Point.Forecast'))
accuracy13$diff <- (accuracy13$Point.Forecast-accuracy13$VALUE)/accuracy13$VALUE

library(plotly)
fig13 <- plotly::plot_ly(accuracy13,  y = ~VALUE, x= ~REF_DATE, name = 'Actual Value', 
                        type = 'scatter', mode = 'lines', 
                        line = list(color = 'rgb(15, 19, 167)', width = 2))  %>% 
  add_trace(y = ~Point.Forecast, name = 'Forecasted', mode = 'lines', 
            line = list(color = 'rgb(100, 39, 67)', width = 2, dash = 'dash')) %>%
  layout(title = "Forecasted Electricity generation (NU 2019-2020)",
         xaxis = list(title = "Months"),
         yaxis = list (title = "Electricity"))

fig13

write.csv(accuracy13, "Forecast_with_confidence_NU.csv")
#-----------------------------------------------------------------------------------------------------



#----Accuracy Metrics

mean(accuracy1$diff, na.rm = T) #Newfoundland & Labrador 
mean(accuracy2$diff, na.rm = T) #Northwest Territories
mean(accuracy3$diff, na.rm = T) #Prince Edward Island
mean(accuracy4$diff, na.rm = T) #Nova Scotia
mean(accuracy5$diff, na.rm = T) #New Brunswick
mean(accuracy6$diff, na.rm = T) #Quebec
mean(accuracy7$diff, na.rm = T) #Ontario
mean(accuracy8$diff, na.rm = T) #Manitoba
mean(accuracy9$diff, na.rm = T) #Saskatchewan
mean(accuracy10$diff, na.rm = T) #Alberta
mean(accuracy11$diff, na.rm = T) #British Columbia
mean(accuracy12$diff, na.rm = T) #Yukon
mean(accuracy13$diff, na.rm = T) #Nunavut

accuracy(NFL_Gen_ts_reg)
accuracy(NWT_Gen_ts_reg)
accuracy(PEI_Gen_ts_reg)
accuracy(NS_Gen_ts_reg)
accuracy(NB_Gen_ts_reg)
accuracy(QB_Gen_ts_reg)
accuracy(ON_Gen_ts_reg)
accuracy(MA_Gen_ts_reg)
accuracy(SA_Gen_ts_reg)
accuracy(Al_Gen_ts_reg)
accuracy(BC_Gen_ts_reg)
accuracy(YU_Gen_ts_reg)
accuracy(NU_Gen_ts_reg)