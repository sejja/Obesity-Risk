library(faraway)  # For data (assuming it contains 'obesity_data' dataset)
library(arules)  # For association rules mining (not used in the provided code)
library(ResourceSelection)  # For model selection (not used in the provided code)
library(caret)  # For machine learning model evaluation
library(pROC)  # For ROC curve analysis

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
