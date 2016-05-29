#This script creates a fake data set that is used to test the viability of a novel issue detection methodology
#on data that presents a known issue. It is heavily documented to communicate any assumptions that have been
#made and any constraints that have been placed on the data, and the reasoning behind each.


library(magrittr)
library(plyr)
library(tidyr)
library(owactools)
library(lubridate)
library(equivalence)
library(dplyr)

setwd("//Ad.us.mrshmc.com/us_users/CHI/DZORNEK/Bitbucket Repository/Issue Detection")

data <- read.csv("data/Revised data.csv", stringsAsFactors = FALSE)
exposure <- read.csv("data/Revised Exposure Workup.csv", stringsAsFactors = FALSE)

#In insurance data, I deal with two orthogonal time variables: Maturity, ie. the age of insurance claims, 
#and the age of an insurance policy at the time claims are reported. I am only concerned with effects
#due to policy age at this stage of modeling, so I remove claims that are too mature, in order to avoid bias in
#data from older policy years, which contain more mature claims than recent policy years. I subtract 2
#from the Report Date to correct discrepancies betIen Excel's and R's date counting.

data %<>%
  mutate(Reported_Date = as.Date(Report.Date - 2, origin = "1900-01-01"),
         Too_Mature_Flag = ifelse(year(Reported_Date) > Policy.Year, "Drop", "Keep")) %>% 
  filter(Too_Mature_Flag == "Keep", 
         !is.na(Benefit.State)) %>% 
  select(Policy_Year = Policy.Year, LOB, Benefit_State = Benefit.State, Reported_Count = Rpt.T)

#Aggregate claim and exposure data to a smaller set of variables for simplicity while ironing out kinks
#in the methodology.

Aggregated_Data <- data %>% 
  group_by(LOB, Benefit_State, Policy_Year) %>% 
  summarise(Reported_Count = sum(Reported_Count))

Aggregated_Exposure <- exposure %>% 
  group_by(LOB.Final, State, Year) %>% 
  summarise(FTE = sum(FTE)) %>% 
  rename(Benefit_State = State, Policy_Year = Year, LOB = LOB.Final)

#Scrub data of missing values and calculate claim frequency. Frequency is scaled up with a factor of 100, so that we
#actually have frequency per 100 full-time employees. The only real reason for this is to reduce the number of decimals
#we have to deal with in reading the numbers.

Real_Data_Before <- Aggregated_Data %>% 
  left_join(Aggregated_Exposure) %>% 
  filter(!is.na(LOB), !is.na(Benefit_State), !is.na(FTE)) %>% 
  mutate(Frequency = 100*Reported_Count/FTE) %>% 
  select(LOB, Benefit_State, Policy_Year, Reported_Count, FTE, Frequency)


#First I simulate a known, very simple, claim performance issue in the data. I simulate a line of business
#whose worker's compensation claim frequency is significantly higher than average for the entire book of business, then simulate a
#corporate restructuring in which employees are moved to the more risky business unit. For experimental design purposes, I hold
#the total number of employees across all business units constant. I also hold the distribution of employees across the non-simulated
#business units the same.


#There are two questions that need to be answered at this stage:
#(1) How much worse is the simulated business unit than the norm?
#(2) How many employees move to the simulated business unit?

#For (1), I add X standard deviations to F, the average frequecy for the non-simulated business units. This breaks (1) down into:
#(i) What should I choose for X?
#(ii) What is the stadard deviation of F across business units?
#I don't need to be *super* precise in our estimate of the standard deviation of F, since the exact amount of "badness" in the
#simulated business unit isn't the major point of concern at present. What I care about is simply flagging that some amount of badness
#is present.

#In answering (ii), ,ombining all policy years into a single standard deviation calculation will introduce bias if a natural
#frequency trend is causing more recent years to have a higher average frequency than historical years, which is very common
#in insurance data. So the general procedure will be to calculate a standard deviation of F for each policy year, then take
#the average standard deviation across policy years.

#Because our data segments are quite granular, some segments have only one full-time employee. For these segments, even a single worker's
#comp claim will cause the claim frequency to spike to 1 claim per employee per year, which is a clear bias. To eliminate this bias, I
#exclude any segment with 100% frequency. HoIver, I make note of observation (a): Some bias will still remain in the data, since the same
#situation arises for two and three employees, with an apparent 50% and 33% frequency following from a single claim, but I handle these
#judgmentally elsewhere, as I do not want to remove too many business units from our data, and I don't need a super high level of
#accuracy for this.

temp <- filter(Real_Data_Before, Frequency != 100)


#Using the remaining data, I create a table of standard deviations for each policy year
Std_Dev_Frequency <- data.frame(matrix(ncol = 2, nrow = length(unique(temp$Policy_Year))))
names(Std_Dev_Frequency) <- c("Year", "StdDev")
Std_Dev_Frequency$Year <- min(temp$Policy_Year):max(temp$Policy_Year)

#The sd() function gives the *sample* standard deviation, so our standard deviation estimator is unbiased.
for(i in unique(temp$Policy_Year)){
  Std_Dev_Frequency[which(Std_Dev_Frequency$Year == i), 2] <- sd(filter(temp, Policy_Year == i)$Frequency, na.rm = TRUE)
}


Estimated_Std_Dev <- mean(Std_Dev_Frequency$StdDev, na.rm = TRUE)

#Now I turn to answering (i). Assuming normality, I initially select 1.96 ~ 2 standard deviations, since
#this gives us the somewhat standard 5% chance that a frequency this high is due to normal noise in the data.
#HoIver, I do still need to account for observation (a) in some way. In grand hand-wavy fashion, I judgmentally
#select 1.75 instead of 2 to hedge against the bias a bit. I do not believe a more analytical handling of (a)
#is necessary in this context.

X <- 1.75

#This is our overall frequency for the entire data set, also scaled up by a factor of 100.
Overall_Real_Frequency <- 100*sum(temp$Reported_Count)/sum(temp$FTE, na.rm = TRUE)

#We calculate a multiplier that is applied to the overall frequency to determine the frequency for our
#simulated bad line of business. This is our final answer to (1). Note that applying frequency multiplier
#is the same as adding X standard deviations to frequency.
Frequency_Multiplier <- 1 + X*Estimated_Std_Dev/Overall_Real_Frequency

#Now we simulate the bad business unit data prior to the distributional shift.
#I begin with the non-simulated pre-shift data and calculated frequency (applying the frequency multiplier),
#then I set the number of employees equal to one ninth of the number of employees across all other business
#units, so that the simulated business unit accounts for 10% of total employees when combined with the non-simulated
#data. The only criteria for selecting 10% was that it should be large enough to have a significant effect
#on the data as a whole. A lot of different numbers would be acceptable; this particular selection is driven by
#background knowledge about insurance programs. I then recalculate the number of claims based on the "bad" frequency
#and calculated number of employees, but I round these counts to the nearest integer, since claim counts are
#necessarily integer-valued. Then, I recalculate frequency once more, to make it consistent with the rounded
#claim counts. Finally, I name the simulated line of business "ISSUE" and combine it with the non-simulated
#data to arrive at a simulated total data set prior to the distributional shift.

Fake_Data_Before <- Real_Data_Before %>% 
  group_by(Benefit_State, Policy_Year) %>% 
  summarise(Reported_Count = sum(Reported_Count),
            FTE = sum(FTE)) %>% 
  as.data.frame() %>% 
  mutate(Frequency = Frequency_Multiplier*100*Reported_Count/FTE,
         FTE = FTE/9,
         Reported_Count = round(Frequency*FTE/100,0),
         Frequency = 100*Reported_Count/FTE,
         LOB = "ISSUE") %>% 
  select(LOB, Benefit_State, Policy_Year, Reported_Count, FTE, Frequency)

Total_Data_Before <- rbind(Real_Data_Before, Fake_Data_Before)

#Next, I turn to simulating the distributional shift. Selecting an appropriate method of simulation was driven
#by considerations of the granularity of data available to us for testing after all the simulation is done,
#as well as experimental design considerations mandating that we (a) hold the distribution across non-simulated
#lines of business and (b) total number of employees constant through the distributional shift.

#Two methods were considered:
#
#(A) The Claim-removal method moves actual claims associated with the employees that are being moved to
#the fake business unit. This method maintains high granularity of data, since it does not require
#aggregating claims data to the level of granularity available in employee exposure data. However,
#it is less precise in meeting constraint (a) and requires statistically testing a number of simulated
#moves to ensure that the distributions can be regarded as equivalent within an acceptable margin of error.
#The tost() test is used for this purpose.
#
#(B) The Exposure-removal method moves employees to the simulated business unit, which sacrifices some
#granularity, but maintains high precision in meeting constraint (a) without the need for additional
#testing of multiple scenarios.
#
#Ultimately, I chose (B) as the method for these early stages of methodology development. Since I am
#beginning with the univariate case, granularity isn't very important. However, it may become important
#when I generalize to the multivariate case, so I retain the mechanics of method (A) below for reference.

#At this stage, it also becomes necessary to answer question (2). Based on background knowledge of what
#counts as a large distributional shift in worker's comp data, I judgmentally select 10%.

# #### (A) Claim-removal method
#
# # First, I calculate distribution across states and lines of business.
# Original_State_Distribution <- count(data, Benefit_State) %>% 
#   mutate(n = n / nrow(data))
# Original_LOB_Distribution <- count(data, LOB) %>% 
#   mutate(n = n / nrow(data))
# 
# #Next, I simulate 100 different scenarios in which I randomly select 10% of claims to move from
# #the non-simulated business units to the simulated business unit.
#
# Kept <- data.frame(matrix(nrow = round(0.9*nrow(data)), ncol = 100))
# State_Distributions <- data.frame(matrix(nrow = nrow(Original_State_Distribution), ncol = 100))
# LOB_Distributions <- data.frame(matrix(nrow = nrow(Original_LOB_Distribution), ncol = 100))
# 
# for(i in 1:100){
#   names(Kept)[i] <- paste0("Sample_",i)
#   names(State_Distributions)[i] <- paste0("Sample_",i)
#   set.seed(i)
#   Kept[paste0("Sample_",i)] <- sample(nrow(data), round(0.9*nrow(data),0))
#   State_Distributions[paste0("Sample_",i)] <- count(slice(data, Kept[,paste0("Sample_",i)]), Benefit_State)$n / round(0.9*nrow(data),0)
#   LOB_Distributions[paste0("Sample_",i)] <- count(slice(data, Kept[,paste0("Sample_",i)]), LOB)$n / round(0.9*nrow(data),0)
# }
# 
# #Testing Equivalence of Distributions for each scenario, set at a confidence level of 99%.
# 
# State_0.99 <- data.frame(matrix(nrow = 100, ncol = 1))
# LOB_0.99 <- data.frame(matrix(nrow = 100, ncol = 1))
# 
# for(i in 1:100){
#   State_0.99[i,1] <- tost(Original_State_Distribution$n, State_Distributions[,paste0("Sample_",i)], paired = TRUE, conf.level = 0.99)$result
#   LOB_0.99[i,1] <- tost(Original_LOB_Distribution$n, LOB_Distributions[,paste0("Sample_",i)], paired = TRUE, conf.level = 0.99)$result
# }


#### (B) Exposure-removal method

#Create adjusted employee non-simulated data. I am simulating the distributional shift to occur in policy year 2015, so I
#leave 2010-2014 as-is and remove 10% of employees from non=simulated business units in 2015. This meets criterion (a).
Adjusted_Exposure <- Aggregated_Exposure %>% 
  mutate(Adjusted_FTE = ifelse(Policy_Year == 2015, 0.9*FTE, FTE)) %>% 
  select(-FTE)

#Next, I use the adjusted employee data to adjust claim data for non-simulated business units.
Real_Data_After_ExposureAdj <- Real_Data_Before %>% 
  left_join(Adjusted_Exposure) %>%
  filter(!is.na(Adjusted_FTE)) %>% 
  mutate(FTE = Adjusted_FTE,
         Reported_Count = round(FTE*Frequency/100,0)) %>% 
  select(-Adjusted_FTE)

#Now that employees have been removed from the non-simulated business units, I must add them to the simulated business unit.
#In order to ensure that criterion (b) is met, I derive an adjust factor:
# y = percent of employees removed from non-simulated business units = 10%
# w = number of employees in non-simulated business units before shift
# x1 = number of employees in simulated business unit before shift = w/9
# x2 = number of employees in simulated business unit after shift
# Total number of employees = w + x1 = (1 - y)w + x2 = 0.9w + x2 -> x2 = 0.1w + x1 = 0.1(9*x1) + x1 = 1.9*x1
# 
# I adjust the number of employees in the simulated business unit by a factor of 1.9 for policy year 2015.

Adjustment_Factor <- 1.9

Fake_Data_After_ExposureAdj <- Fake_Data_Before %>% 
  mutate(FTE = ifelse(Policy_Year == 2015, Adjustment_Factor*FTE, FTE),
         Reported_Count = round(FTE*Frequency/100, 0))

# I combine the non-simulated data with the simulated data to arrive at a total data set that simulates
# a distributional shift to a more risky business unit. Viola!
Total_Data_After_ExposureAdj <- rbind(Real_Data_After_ExposureAdj, Fake_Data_After_ExposureAdj)

saveRDS(Total_Data_After_ExposureAdj, file = "Adjusted_Data.rds")