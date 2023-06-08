library(dplyr)
library("glm2")
#library(tidyverse)
library(tidymodels)

# Import the data
data <- read.csv("20230501_i3_Pipeline.csv")

# Remove these columns
df <- data[ , !names(data) %in%
               c("ID","NAME","GovWin.ID","Capture.Lead","Include.in.Moneyball","Expected.RFP.Release",
                 "Prime.Contractor","Must.Win","Status","Actual.RFP.Release","Award.Date",
                 "BD_Sales.Lead", "Contract.Vehicle","Contracts.POC","Estimated.Start.Date","Modified.By",
                 "Moneyball","NAICS", "Prime.POC","Prime.POC.Email","RFI.or.IWP","Proposal.or.EWP.Due",
                 "Proposals.POC","TA.completed","Submission.Date","Estimated.Completion.Date","NAME.1",
                 "Solicitation..","Solicitation.Date","Technical.POC")]


# Add a new column Result column that is 1 for "WON" and 0 for all else.
df$Result <- with(df, ifelse(Stage == "WON", 1, 0))

# Replace NB - RFQ with NB
df$Customer.Type[df$Customer.Type == "NB - RFQ"] <- "NB"
df$Customer.Type[df$Customer.Type == "RC - RFQ"] <- "RC"

# Remove records with Stages other than "WON", "LOST", or "CANCEL"
df <- df[!(df$Stage=="INACTIVE"|df$Stage=="PROPSUB"|df$Stage=="CAPTURE"|df$Stage=="PROPPREP"|df$Stage=="QUAL"|df$Stage=="IDENT"),]
# Remove Stage column
df <- df[ , !names(df) %in% c("Stage")]

str(df) # View structure of data frame

# Convert columns from Characters to Currency, Percentages, or Numbers
df$Total.Value <- as.numeric(gsub('[$,]', '', df$Total.Value))
df$Our.Value <- as.numeric(gsub('[$,]', '', df$Our.Value))
df$Weighted.Value <- as.numeric(gsub('[$,]', '', df$Weighted.Value))
df$Win.Probability = as.integer(sub("%", "",df$Win.Probability))
df$Contract.Period.Months <- as.numeric(df$Contract.Period.Months)

str(df) # View structure of data frame

# 
# #Convert the target variable to a factor
# df$Result = as.factor(df$Result)

# Split data in training and test sets
set.seed(509)
split <- initial_split(df, prop = 0.8, strata = Result)
train <- split %>% training()
test <- split %>% testing()


predictors <- c("Portfolio", "Business.Unit", "i3.Role", "Total.Value", "Our.Value", "Win.Probability", 
                "Competition.Type", "Contract.Type", "Customer.Type","Primary.Agency","Contract.Period.Months","CPEG") 
response <- "Result" 


fit.full <- glm2(Result ~ Portfolio + i3.Role + Competition.Type + Contract.Type + Customer.Type + Primary.Agency
                 + Contract.Period.Months + CPEG + Our.Value, data = df, family=binomial())

# Print the summary of the model to check the results
summary(fit.full)

# Reduce the number of variables used in the model.
fit.reduced <- glm2(Result ~ Portfolio + Contract.Type + Customer.Type + Contract.Period.Months + CPEG, data = df, family=binomial())

# Print the summary of the model to check the results
summary(fit.reduced)

# Run chi-square test with anova function in R to compared between first and second model
# to see which model explains our response variable better.
# Non-significant p-value (>0.05) means that the second model fits as well as the full model.
anova(fit.reduced, fit.full, test="Chisq") 

coef(fit.reduced)
exp(coef(fit.reduced)) 

model<- fit.reduced

newdata = test

# Turn off scientific notation
options(scipen = 999)

#use model to predict value of am
newdata$prob <- predict.glm(model, newdata, type="response")

# Check for overdispersion
# If the ratio considerably larger than 1, then it indicates that we have an overdispersion issue.
deviance(fit.reduced)/df.residual(fit.reduced) 
