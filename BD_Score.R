library(dplyr)
library("glm2")
#library(tidyverse)

data <- read.csv("20230501_i3_Pipeline.csv")

# Remove these columns
df1 <- data[ , !names(data) %in%
                c("GovWin.ID","Capture.Lead","Include.in.Moneyball","Expected.RFP.Release","Must.Win","Status","Actual.RFP.Release",
                  "Award.Date","BD_Sales.Lead","Contracts.POC","Estimated.Start.Date","Modified.By","Moneyball","Prime.POC",
                  "Prime.POC.Email","RFI.or.IWP","Proposal.or.EWP.Due","Proposals.POC","TA.completed","Submission.Date",
                  "Estimated.Completion.Date","NAME.1","Solicitation..","Solicitation.Date","Technical.POC")]

# Add a new column Result column that is 1 for "WON" and 0 for all else.
df1$Result <- with(df1, ifelse(Stage == "WON", 1, 0))

# Replace NB - RFQ with NB
df1$Customer.Type[df1$Customer.Type == "NB - RFQ"] <- "NB"
df1$Customer.Type[df1$Customer.Type == "RC - RFQ"] <- "RC"

# Remove records with Stages other than "WON", "LOST", or "CANCEL"
df2 <- df1[!(df1$Stage=="INACTIVE"|df1$Stage=="PROPSUB"|df1$Stage=="CAPTURE"|df1$Stage=="PROPPREP"|df1$Stage=="QUAL"|df1$Stage=="IDENT"),]

str(df2) # View structure of data frame

# Convert columns from Characters to Currency, Percentages, or Numbers
df2$Total.Value <- as.numeric(gsub('[$,]', '', df2$Total.Value))
df2$Our.Value <- as.numeric(gsub('[$,]', '', df2$Our.Value))
df2$Weighted.Value <- as.numeric(gsub('[$,]', '', df2$Weighted.Value))
df2$Win.Probability = as.integer(sub("%", "",df2$Win.Probability))
df2$Contract.Period.Months <- as.numeric(df2$Contract.Period.Months)

str(df2) # View structure of data frame
predictors <- c("Portfolio", "Business.Unit", "i3.Role", "Total.Value", "Our.Value", "Win.Probability", 
                "Competition.Type", "Contract.Type", "Customer.Type","Primary.Agency","Contract.Period.Months","CPEG") 
response <- "Result" 

summary(df2) # View summary


# # Fit the linear regression model using all predictor variables
# fit <- lm(data[[response]] ~ data[[predictors[1]]] + data[[predictors[2]]] + data[[predictors[3]]] + data[[predictors[4]]] 
#           + data[[predictors[5]]] + data[[predictors[6]]] + data[[predictors[7]]] + data[[predictors[8]]] + data[[predictors[9]]] 
#           + data[[predictors[10]]] + data[[predictors[11]]] + data[[predictors[12]]] + data[[predictors[13]]], data = df2)
# 
# # Drop predictor variables with high p-values
# while (any(summary(fit)$coefficients[-1,4] > 0.05)) { # Drop variables one at a time until no variables have a p-value > 0.05
#   drop_var <- names(summary(fit)$coefficients[-1,4])[which.max(summary(fit)$coefficients[-1,4])] # Find the variable with the highest p-value
#   predictors <- predictors[predictors != drop_var] # Drop the variable from the list of predictors
#   fit <- lm(data[[response]] ~ data[[predictors[1]]] + data[[predictors[2]]] + data[[predictors[3]]] + data[[predictors[4]]]
#             + data[[predictors[5]]] + data[[predictors[6]]] + data[[predictors[7]]] + data[[predictors[8]]] + data[[predictors[9]]]
#             + data[[predictors[10]]] + data[[predictors[11]]] + data[[predictors[12]]] + data[[predictors[13]]], data = df2) # Fit the model with the remaining predictors
# }

fit.full <- glm2(Result ~ Portfolio + i3.Role + Competition.Type + Contract.Type + Customer.Type + Primary.Agency
           + Contract.Period.Months + CPEG, data = df2, family=binomial())

# Print the summary of the model to check the results
summary(fit.full)


#fit.reduced <- glm2(Result ~ Portfolio + i3.Role + Contract.Type + Customer.Type + Contract.Period.Months, data = df2, family=binomial())

fit.reduced <- glm2(Result ~ Portfolio + Contract.Type + Customer.Type + Contract.Period.Months + CPEG, data = df2, family=binomial())

# Print the summary of the model to check the results
summary(fit.reduced)

# Run chi-square test with anova function in R to compared between first and second model
# to see which model explains our response variable better.
anova(fit.reduced, fit.full, test="Chisq") 

coef(fit.reduced)
exp(coef(fit.reduced)) 

model<- fit.reduced

#define new observation
newdata = data.frame(Portfolio = "CREWS", Contract.Type = "TASKORDER", Customer.Type = "RC", Contract.Period.Months = 48, CPEG = "Expand")

#use model to predict value of am
predict.glm(model, newdata, type="response")