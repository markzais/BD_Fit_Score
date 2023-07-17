library(dplyr)
library("glm2")
#library(tidyverse)
library(tidymodels)
library(ggplot2)
library(gridExtra)
library(pROC)
library(writexl)

path <- getwd()

# Import the data
data <- read.csv("i3_Pipeline.csv")

# Convert columns from Characters, Currency & Percentages to Numbers
data$Total.Value <- as.numeric(gsub('[$,]', '', data$Total.Value))
data$Our.Value <- as.numeric(gsub('[$,]', '', data$Our.Value))
data$Weighted.Value <- as.numeric(gsub('[$,]', '', data$Weighted.Value))
data$Win.Probability = as.integer(sub("%", "",data$Win.Probability))
data$Contract.Period.Months <- as.numeric(data$Contract.Period.Months)


# Add a new column Result column that is 1 for "WON" and 0 for all else.
data$Result <- with(data, ifelse(Stage == "WON", 1, 0))

# Replace NB - RFQ with NB
data$Customer.Type[data$Customer.Type == "NB - RFQ"] <- "NB"
data$Customer.Type[data$Customer.Type == "RC - RFQ"] <- "RC"

# Remove these columns
df <- data[ , !names(data) %in%
               c("NAME","GovWin.ID","Capture.Lead","Include.in.Moneyball","Expected.RFP.Release",
                 "Prime.Contractor","Must.Win","Status","Actual.RFP.Release","Award.Date",
                 "BD_Sales.Lead", "Contract.Vehicle","Contracts.POC","Estimated.Start.Date","Modified.By",
                 "Moneyball", "Prime.POC","Prime.POC.Email","RFI.or.IWP","Proposal.or.EWP.Due",
                 "Proposals.POC","TA.completed","Submission.Date","Estimated.Completion.Date","NAME.1",
                 "Solicitation..","Solicitation.Date","Technical.POC")]

# Remove records with Stages other than "WON", "LOST", or "CANCEL"
df <- df[!(df$Stage=="INACTIVE"|df$Stage=="PROPSUB"|df$Stage=="CAPTURE"|df$Stage=="PROPPREP"|df$Stage=="QUAL"|df$Stage=="IDENT"),]
# Remove Stage column
df <- df[ , !names(df) %in% c("Stage")]

str(df) # View structure of data frame

# # Create a picture (png) of the first 10 rows for PowerPoint slides
# temp <- head(df,10)
# png("temp.png", height=300, width=1500)
# p<-tableGrob(temp)
# grid.arrange(p)
# dev.off()

# Split data in training and test sets
set.seed(509)
split <- initial_split(df, prop = 0.8, strata = Result)
train <- split %>% training()
test <- split %>% testing()

# predictors <- c("Portfolio", "Business.Unit", "i3.Role", "Total.Value", "Our.Value", "Win.Probability", 
#                 "Competition.Type", "Contract.Type", "Customer.Type","Primary.Agency","Contract.Period.Months","CPEG") 
# response <- "Result" 

fit.full <- glm2(Result ~ Portfolio + i3.Role + Competition.Type + Contract.Type + Customer.Type +  NAICS + Primary.Agency
                 + Contract.Period.Months + CPEG + Our.Value, data = df, family=binomial())

# Print the summary of the model to check the results
summary(fit.full)

# Reduce the number of variables used in the model.
# fit.reduced <- glm2(Result ~ Portfolio + Contract.Type + Customer.Type + NAICS + Contract.Period.Months + CPEG, data = df, family=binomial())
fit.reduced <- glm2(Result ~ Portfolio + Our.Value + Contract.Type + Customer.Type + Contract.Period.Months, data = df, family=binomial())


# Print the summary of the model to check the results
summary(fit.reduced)

# Run chi-square test with anova function in R to compared between first and second model
# to see which model explains our response variable better.
# Non-significant p-value (>0.05) means that the second model fits as well as the full model.
anova(fit.reduced, fit.full, test="Chisq") 

coef(fit.reduced)
exp(coef(fit.reduced)) 

model<- fit.reduced

# View summary of model
tidy(model)

newdata = test

# #define new observation
# newdata = data.frame(Portfolio = "CREWS", Contract.Type = "TASKORDER", Customer.Type = "RC", Contract.Period.Months = 12, CPEG = "Expand")
# 

# Turn off scientific notation
options(scipen = 999)

#use model to predict value of a win
newdata$prob <- predict.glm(model, newdata, type="response")

data$prob <- predict.glm(model, data, type="response")
output_path <- paste0(path,"/Output/")
write_xlsx(data,paste0(output_path,"model_output.xlsx"))

# Plot the Receiver Operating Characteristic (ROC) curve
predicted_probs <- predict(model, type = "response")
actual_outcomes <- df$Result
roc_obj <- roc(actual_outcomes, predicted_probs)
plot(roc_obj, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")

# Caluclate AUC (Area Under the Curve): Closer to 1 is better.
auc_value <- auc(roc_obj)

# #use model to predict value of a win
# prediction <- predict.glm(model, newdata, type="response")

# Check for overdispersion
# If the ratio considerably larger than 1, then it indicates that we have an overdispersion issue.
deviance(fit.reduced)/df.residual(fit.reduced) 

#################################
#################################
#    Model Method #2            #
#################################
#################################

df2 <- df

#Convert the target variable to a factor
df2$Result = as.factor(df2$Result)

# Split data in training and test sets
set.seed(509)
split <- initial_split(df2, prop = 0.8, strata = Result)
train2 <- split %>% training()
test2 <- split %>% testing()

# Train a model
model2 <- logistic_reg(mixture = double(1), penalty = double(1)) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(Result ~ ., data=train2)
# 
# newdata = data.frame(Portfolio = "CREWS", Contract.Type = "TASKORDER", Customer.Type = "RC", Contract.Period.Months = 12, CPEG = "Expand")

# Make predictions
pred_class <- predict(model2, new_data = test2, type = "class")

pred_proba <- predict(model2, new_data = test2, type = "prob")

results <- test2 %>% select(Result) %>% bind_cols(pred_class, pred_proba)

accuracy(results, truth = Result, estimate = .pred_class)

# Create confusion matrix
conf_mat(results, truth = Result,
         estimate = .pred_class)
