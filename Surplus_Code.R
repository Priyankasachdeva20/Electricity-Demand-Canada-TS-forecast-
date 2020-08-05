

setwd("P:/Courses/MMA 860 - Alexander Scott Acquisition and Data Management/Project/TS/Ontario_Demand")

library(readxl)

file <- read_excel("Surplus_Analysis_On.xlsx")


str(file)

file1 <- file %>% group_by(Date) %>% summarise(total_output=sum(`Total Output`), 
                                                     total_demand=sum(`Ontario Demand`))

file1$Difference <- file1$total_output-file1$total_demand


library(plotly)

fig <- plotly::plot_ly(file1,  y = ~total_output, x= ~Date, name = 'Ontario_Supply', 
                       type = 'scatter', mode = 'lines', 
                       line = list(color = 'rgb(15, 19, 167)', width = 2))  %>% 
  add_trace(y = ~total_demand, name = 'Ontario_Demand', mode = 'lines', 
            line = list(color = 'rgb(100, 39, 67)', width = 2)) %>%
  add_trace(y = ~Difference, name = 'Surplus', mode = 'lines', 
            line = list(color = 'rgb(100, 539, 67)', width = 2))%>%
  layout(title = "Impact of COVID-19 on Electricity demand and consumption",
         xaxis = list(title = "Days"),
         yaxis = list (title = "Electricity"))

fig


write.csv(file1, "Output.csv")
