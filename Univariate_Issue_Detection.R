#This script carries out the steps of a novel method for detecting when adverse experience is indicative of some
#underlying issue that should be investigated, as opposed to random noise. The problem addressed is a hyrid of
#signal detection and anomaly detection problems, but it is compounded by the fact that no known issues are present
#in historical data to learn from. The method involves both Bayesian and frequentist statistics, and the conflict
#leads to an interesting issue with determining an appropriate sensitivity and specificity, which can be understood
#through the accompanying Shiny app.

#This univariate test is intended as a first step toward generalization into a full-scale multivariate issue
#detection methodology.

#The general method flows like this:
#At each data evaluation date, perform Bayesian updating on data to arrive at a posterior distribution for claim
#frequency parameters. If the posterior expected claim frequency falls outside of some confidence interval under the
#prior distribution, we flag it as a possible issue. Selection of an appropriate confidence interval is the key point
#of judgment, which merits special handling on its own. In this script, I assume a 90% confidence interval. In
#a real-world problem, the confidence interval can be determined using insights from the accompanying Shiny app.

#An alternate methodology was also considered, but is not presented here. In the alternate methodology, a GLM is fit
#to total data as of 2014 and 2015, and then model parameters are compared to determine whether an issue is flagged.
#This method has not turned out to be fruitful in the case of this particular case study, but perhaps other studies
#exist in which it yields better results than the Bayesian approach. If such studies can be formulated, they may
#help to inform our understanding of both methods, especially when one might be preferred to the other.

library(Hmisc)
library(magrittr)
library(ggplot2)
library(plyr)
library(dplyr)

data <- readRDS("data/Adjusted_Data.rds") %>% 
  ungroup()

#Separate out current year data from prior data.
New_Year <- data %>% 
  filter(Policy_Year == 2015)

Prior_Data <- data %>% 
  filter(Policy_Year != 2015)

#Claim frequency is typically modeled as a poisson distribution. Because of the simplicity
#it affords, I assume a gamma conjugate prior for the poisson parameter.

#Estimates of inital gamma parameters using policy year 2010.
#Mean = alpha / beta; Variance = alpha / beta^2
data10 <- data %>% 
  filter(Policy_Year == 2010) %>% 
  mutate(Frequency = Frequency/100)

mean <- wtd.mean(data10$Frequency, weights = data10$FTE)
var <- wtd.var(data10$Frequency, weights = data10$FTE)

beta <- mean / var
alpha <- mean * beta

Initial_Lambda <- alpha / beta

#Bayesian estimates of Lambda, alpha, and beta at each data evaluation year,
#drawing on the conjugate relationship between poisson and gamma. I also
#include the percent change in lambda at each update, for infomrational
#purposes.

Bayes_Lambda <- data %>% 
  filter(Policy_Year != 2010) %>% 
  group_by(Policy_Year) %>% 
  summarise(Reported_Count = sum(Reported_Count),
            FTE = sum(FTE)) %>% 
  mutate(Cumlative_Reported = cumsum(Reported_Count),
         Cumlative_FTE = cumsum(FTE),
         Bayes_alpha = alpha + Cumlative_Reported,
         Bayes_beta = beta + Cumlative_FTE,
         Estimated_Lambda = Bayes_alpha / Bayes_beta,
         Lambda_Lag = lag(Estimated_Lambda, 1, default = Initial_Lambda),
         Percent_Change = (Estimated_Lambda / Lambda_Lag) - 1)

Var_Lambda <- c()

for(i in 1:nrow(Bayes_Lambda)){
  lambdas <- c(Initial_Lambda, Bayes_Lambda$Estimated_Lambda)
  
  Var_Lambda[i] <- print(var(lambdas[1:(i+1)]))
  
}

Bayes_Lambda$Var_Lambda <- Var_Lambda

## 95% confidence interval for lambda as of 2014, assuming 90% confidence.
CI <- qgamma(c(0.05, 0.95), shape = alpha, rate = beta)


#Bayesian estimate of the New_Year lambda, utilizing the poisson-gamma conjugate relationship.
Lambda_2015 <- 100*sum(New_Year$Reported_Count) / sum(New_Year$FTE)


#Perform the test. A TRUE value indicates that an issue was detected in 2015. FALSE indicates otherwise.
#Note: in this test case, with a 90% confidence assumption, we return a FALSE result.The accompanying Shiny
#app allows the user to play with various simulation assumption to determine what is necessary to return a
#TRUE result. In real-world applications, quite low confidence assumptions may be acceptable depending on the
#costs associated with type 1 and type 2 errors. With insurance data, the cost of failing to flag an issue
#that exists is far greater than the cost of erroneously flagging a non-existent issue, so we are able
#to accept quite narrow confidence intervals in practice.

Issue_Flagged <- Lambda_2015 > CI[2]




