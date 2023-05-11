library(dplyr)
library("glm2")
#library(tidyverse)
library(tidymodels)

# Import the data
data <- read.csv("20230501_i3_Pipeline.csv")

#################
# PREP THE DATA #
#################

# Remove these columns
df1 <- data[ , !names(data) %in%
                c("ID","NAME","GovWin.ID","Capture.Lead","Include.in.Moneyball","Expected.RFP.Release",
                  "Prime.Contractor","Must.Win","Status","Actual.RFP.Release","Award.Date",
                  "BD_Sales.Lead","Contract.Type","Contract.Vehicle","Contracts.POC","Estimated.Start.Date","Modified.By",
                  "Moneyball","NAICS", "Primary.Agency","Prime.POC","Prime.POC.Email","RFI.or.IWP","Proposal.or.EWP.Due",
                  "Proposals.POC","TA.completed","Submission.Date","Estimated.Completion.Date","NAME.1",
                  "Solicitation..","Solicitation.Date","Technical.POC")]

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

#Convert the target variable to a factor
df2$Result = as.factor(df2$Result)

##################
# VIEW SOME DATA #
##################

# # Plot the Result against one variable [Contract Type]
# ggplot(df2, aes(Contract.Type, fill = Result)) +
#   geom_bar() + coord_flip()


# Plot the Result against one variable [Portfolio]
ggplot(df2, aes(Portfolio, fill = Result)) +
  geom_bar() + coord_flip()

##################
# FIT THE MODEL  #
##################

# Split data in training and test sets
set.seed(509)
split <- initial_split(df2, prop = 0.8, strata = Result)
  train <- split %>% training()
  test <- split %>% testing()

# Train a model
model <- logistic_reg(mixture = double(1), penalty = double(1)) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(Result ~ ., data=train)

# View summary of model
tidy(model)


# Make predictions
pred_class <- predict(model, new_data = test, type = "class")

pred_proba <- predict(model, new_data = test, type = "prob")

results <- test %>% select(Result) %>% bind_cols(pred_class, pred_proba)

accuracy(results, truth = Result, estimate = .pred_class)

# Create confusion matrix
conf_mat(results, truth = Result,
         estimate = .pred_class)


###############################
# LETS TRY A DIFFERENT METHOD #
###############################  

# Remove these columns
df3 <- data[ , !names(data) %in%
               c("ID","NAME","GovWin.ID","Capture.Lead","Include.in.Moneyball","Expected.RFP.Release",
                 "Prime.Contractor","Must.Win","Status","Actual.RFP.Release","Award.Date",
                 "BD_Sales.Lead", "Contract.Vehicle","Contracts.POC","Estimated.Start.Date","Modified.By",
                 "Moneyball","NAICS", "Prime.POC","Prime.POC.Email","RFI.or.IWP","Proposal.or.EWP.Due",
                 "Proposals.POC","TA.completed","Submission.Date","Estimated.Completion.Date","NAME.1",
                 "Solicitation..","Solicitation.Date","Technical.POC")]

# Add a new column Result column that is 1 for "WON" and 0 for all else.
df3$Result <- with(df3, ifelse(Stage == "WON", 1, 0))

predictors <- c("Portfolio", "Business.Unit", "i3.Role", "Total.Value", "Our.Value", "Win.Probability", 
                "Competition.Type", "Contract.Type", "Customer.Type","Primary.Agency","Contract.Period.Months","CPEG") 
response <- "Result" 

summary(df3) # View summary


fit.full <- glm2(Result ~ Portfolio + i3.Role + Competition.Type + Contract.Type + Customer.Type + Primary.Agency
           + Contract.Period.Months + CPEG, data = df3, family=binomial())

# Print the summary of the model to check the results
summary(fit.full)


#fit.reduced <- glm2(Result ~ Portfolio + i3.Role + Contract.Type + Customer.Type + Contract.Period.Months, data = df2, family=binomial())

fit.reduced <- glm2(Result ~ Portfolio + Contract.Type + Customer.Type + Contract.Period.Months + CPEG, data = df3, family=binomial())

# Print the summary of the model to check the results
summary(fit.reduced)

# Run chi-square test with anova function in R to compared between first and second model
# to see which model explains our response variable better.
# Non-significant p-value (>0.05) means that the second model fits as well as the full model.
anova(fit.reduced, fit.full, test="Chisq") 

coef(fit.reduced)
exp(coef(fit.reduced)) 

model<- fit.reduced

### Make a new prediction ####
#define new observation
newdata = data.frame(Portfolio = "CREWS", Contract.Type = "TASKORDER", Customer.Type = "RC", Contract.Period.Months = 48, CPEG = "Expand")

#use model to predict value of am
predict.glm(model, newdata, type="response")