library(magrittr)
library(rCharts)
library(DT)
library(plyr)
library(tidyr)
library(owactools)
library(lubridate)
library(equivalence)
library(shiny)
library(shinydashboard)
library(Hmisc)
library(ggplot2)
library(dplyr)


Real_Data_Before <- readRDS("data/Real_Data_Before.rds")
Aggregated_Exposure <- readRDS("data/Aggregated_Exposure.rds")

temp <- filter(Real_Data_Before, Frequency != 100)

Std_Dev_Frequency <- data.frame(matrix(ncol = 2, nrow = length(unique(temp$Policy_Year))))
names(Std_Dev_Frequency) <- c("Year", "StdDev")
Std_Dev_Frequency$Year <- min(temp$Policy_Year):max(temp$Policy_Year)

for(i in unique(temp$Policy_Year)){
  Std_Dev_Frequency[which(Std_Dev_Frequency$Year == i), 2] <- sd(filter(temp, Policy_Year == i)$Frequency, na.rm = TRUE)
}

Overall_Real_Frequency <- 100*sum(temp$Reported_Count)/sum(temp$FTE, na.rm = TRUE)

Max_Policy_Year <- max(Real_Data_Before$Policy_Year, na.rm = TRUE)

