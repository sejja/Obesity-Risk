#
# Regression.R
# Obesity Risk
#
# Created by Diego Revilla on 26/04/24
# Copyright � 2024 . All Rights reserved
# 

############################################################################################

# LIBRARIES


library(ggplot2) # For some basic plotting while exploring the data
library(faraway)  # For data (assuming it contains 'obesity_data' dataset)
library(arules)  # For association rules mining (not used in the provided code)
library(ResourceSelection)  # For model selection (not used in the provided code)
library(caret)  # For machine learning model evaluation
library(pROC)  # For ROC curve analysis
library(dplyr) # For data manipulation


############################################################################################

# DATA LOADING



obesity_data_loading <- function(path) {
  data <- read.csv(path)
  
  factorized_columns <- c('Gender', 'CAEC', 'CALC', 'MTRANS', 'NObeyesdad')
  booleanized_columns <- c('FAVC', 'family_history_with_overweight', 'SMOKE', 'SCC')
  lapply(factorized_columns, function(x) {data[[x]] <<- factor(data[[x]])})  
  lapply(booleanized_columns, function(x) {data[[x]] <<- data[[x]] == 'yes'})  
  
  return(data)  
}

obesity_data <- obesity_data_loading('Datasets/ObesityDataSet.csv')





############################################################################################

# LINEAR REGRESSION

data_linear <- obesity_data

# Assuming your dataset is called 'obesity_data' and you want to include only numeric variables

# Select only numeric variables
numeric_data <- obesity_data[, sapply(obesity_data, is.numeric)]

# Compute correlation matrix
correlation_matrix <- cor(numeric_data)

# Build dummy variables
data_linear$CALC <- ifelse(data_linear$CALC == 'Frequently' | data_linear$CALC == 'Always',1,0)
data_linear$CAEC <- ifelse(data_linear$CAEC == 'Frequently' | data_linear$CAEC == 'Always',1,0)
data_linear$MTRANS<- ifelse(data_linear$MTRANS == 'Automobile' | data_linear$CALC == 'Public_Transport',1,0)

# Calculate different linear regression models by backwards elimination
lm0 <- lm(Weight ~ Gender + Age + Height + family_history_with_overweight + FAVC + FCVC + NCP + CAEC + SMOKE + CH2O + SCC + FAF + TUE + CALC + MTRANS ,data = data_linear)
summary(lm0) # Exclude SMOKE variable from our model

lm1 <- lm(Weight ~ Gender + Age + Height + family_history_with_overweight + FAVC + FCVC + NCP + CAEC +  CH2O + SCC + FAF + TUE + CALC + MTRANS ,data = data_linear)
summary(lm1) # Exclude CALC variable from our model

lm2 <- lm(Weight ~ Gender + Age + Height + family_history_with_overweight + FAVC + FCVC + NCP + CAEC +  CH2O + SCC + FAF + TUE + MTRANS ,data = data_linear)
summary(lm2) # Exclude NCP variable from our model

lm3 <- lm(Weight ~ Gender + Age + Height + family_history_with_overweight + FAVC + FCVC +  CAEC +  CH2O + SCC + FAF + TUE + MTRANS ,data = data_linear)
summary(lm3) # We are also going to exclude Gender (Explanation on the report)

lm4 <- lm(Weight ~  Age + Height + family_history_with_overweight + FAVC + FCVC +  CAEC +  CH2O + SCC + FAF + TUE + MTRANS ,data = data_linear)
summary(lm4)

# Let's study our refined model in detail
linear_regression_model <- lm4

# Calculate the confidence interval
confint(linear_regression_model)

# Summarize the regression results
anova(linear_regression_model)

# Plot the predicted weight against the actual weight
plot(obesity_data$Weight, fitted(linear_regression_model), main = "Actual vs. Predicted", xlab = "Actual Weight", ylab = "Predicted Weight")
abline(0, 1, col = "red")  # Add a diagonal line for perfect prediction

# Let's check the normality of Residuals
plot(linear_regression_model, 2)

# Let's check the influential points with Cook's distance
plot(linear_regression_model, 4)


############################################################################################

# LOGISTIC REGRESSION

data_logistic <- obesity_data

# Convert the response variable to binary: Obesity_Type_III and Obesity_Type_II are marked as 1, others as 0
data_logistic$NObeyesdad <- ifelse(data_logistic$NObeyesdad == 'Obesity_Type_III' | data_logistic$NObeyesdad == 'Obesity_Type_II', 1, 0)

# Remove rows with missing values
data_logistic <- data_logistic[complete.cases(data_logistic),]

# Fit logistic regression model
model <- glm(NObeyesdad ~ NCP + CH2O + FAF + TUE, family = binomial(), data = data_logistic)

# Fit additional logistic regression models with different combinations of predictor variables
model1 <- glm(NObeyesdad ~ NCP + CH2O + FAF + TUE + Age + Height + CAEC + MTRANS + CALC, family = binomial(), data = data_logistic)
model2 <- glm(NObeyesdad ~ NCP + CH2O + FAF + FCVC + TUE, family = binomial(), data = data_logistic)
model3 <- glm(NObeyesdad ~ NCP + CH2O + FAF + Height + Age + TUE, family = binomial(), data = data_logistic)
model4 <- glm(NObeyesdad ~ NCP + CH2O + FAF + Age + TUE, family = binomial(), data = data_logistic)
model5 <- glm(NObeyesdad ~ NCP + CH2O + FAF + Height + TUE, family = binomial(), data = data_logistic)
model6 <- glm(NObeyesdad ~ NCP + CH2O + FAF + TUE + Weight, family = binomial(), data = data_logistic)

# Calculate likelihood ratio statistic and p-value for model comparison
lratiostat <- model1$null.deviance - model$deviance
dofstat <- model1$df.null - model$df.residual
pchisq(lratiostat, df = dofstat, lower.tail = FALSE)

# Calculate likelihood ratio statistic and p-value for model comparison
lratiostat <- model2$null.deviance - model$deviance
dofstat <- model2$df.null - model$df.residual
pchisq(lratiostat, df = dofstat, lower.tail = FALSE)

# Calculate likelihood ratio statistic and p-value for model comparison
lratiostat <- model3$null.deviance - model$deviance
dofstat <- model3$df.null - model$df.residual
pchisq(lratiostat, df = dofstat, lower.tail = FALSE)

# Calculate likelihood ratio statistic and p-value for model comparison
lratiostat <- model4$null.deviance - model$deviance
dofstat <- model4$df.null - model$df.residual
pchisq(lratiostat, df = dofstat, lower.tail = FALSE)

# Calculate likelihood ratio statistic and p-value for model comparison
lratiostat <- model5$null.deviance - model$deviance
dofstat <- model5$df.null - model$df.residual
pchisq(lratiostat, df = dofstat, lower.tail = FALSE)

# Calculate likelihood ratio statistic and p-value for model comparison
lratiostat <- model6$null.deviance - model$deviance
dofstat <- model6$df.null - model$df.residual
pchisq(lratiostat, df = dofstat, lower.tail = FALSE)

# Conduct Hosmer-Lemeshow test for model diagnosis
hoslem.test(data_logistic$NObeyesdad, model$fitted.values)
hoslem.test(data_logistic$NObeyesdad, model1$fitted.values)
hoslem.test(data_logistic$NObeyesdad, model2$fitted.values)
hoslem.test(data_logistic$NObeyesdad, model3$fitted.values)
hoslem.test(data_logistic$NObeyesdad, model4$fitted.values)
hoslem.test(data_logistic$NObeyesdad, model5$fitted.values)
hoslem.test(data_logistic$NObeyesdad, model6$fitted.values)

# Summarize model coefficients and interpret the exponentiated coefficients
summary(model1)
exp(model1$coefficients)

cp <- 0.5 

# Extract response and predicted values
response <- data_logistic$NObeyesdad
prediction <- ifelse(model6$fitted.values < cp, 0, 1)

# Plot ROC curve
par(pty = "s")
Roc <- roc(response, model1$fitted.values, plot = TRUE, legacy.axes = TRUE, print.auc = TRUE)
plot(Roc$thresholds, Roc$sensitivities, type = "l", xlab = "Cut points", ylab = "Sensitivity/Specificity", bty = "n")
lines(Roc$thresholds, Roc$specificities)
text(x = 0.7, y = 0.05, labels = "Sensitivity")
text(x = 0.7, y = 0.95, labels = "Specificity")
abline(v = 0.084, col = "gray", lty = 2)

# Change threshold for classification
cp <- 0.35

# Generate predictions using new threshold
prediction <- ifelse(model1$fitted.values < cp, 0, 1)

# Evaluate model performance using confusion matrix with new threshold
tab <- confusionMatrix(as.factor(prediction), as.factor(response), positive = "1")
tab



