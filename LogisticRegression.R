# Load required libraries
library(faraway)  # For data (assuming it contains 'obesity_data' dataset)
library(arules)  # For association rules mining (not used in the provided code)
library(ResourceSelection)  # For model selection (not used in the provided code)
library(caret)  # For machine learning model evaluation
library(pROC)  # For ROC curve analysis

# Load data
data <- obesity_data

# Convert the response variable to binary: Obesity_Type_III and Obesity_Type_II are marked as 1, others as 0
data$NObeyesdad <- ifelse(data$NObeyesdad == 'Obesity_Type_III' | data$NObeyesdad == 'Obesity_Type_II', 1, 0)
sum(data$NObeyesdad)
# Remove rows with missing values
data <- data[complete.cases(data),]

# Fit logistic regression model
model <- glm(NObeyesdad ~ NCP + CH2O + FAF + TUE, family = binomial(), data = data)
model1 <- glm(NObeyesdad ~ NCP + CH2O + FAF + TUE + Age + Height + CAEC + MTRANS + CALC, family = binomial(), data = data)

# Calculate likelihood ratio statistic
lratiostat <- model1$null.deviance - model$deviance

# Calculate degrees of freedom statistic
dofstat <- model1$df.null - model$df.residual

# Calculate p-value using chi-square distribution
pchisq(lratiostat, df = dofstat, lower.tail = FALSE)

model2 <- glm(NObeyesdad ~ NCP + CH2O + FAF + FCVC + TUE, family = binomial(), data = data)

# Calculate likelihood ratio statistic
lratiostat <- model2$null.deviance - model$deviance

# Calculate degrees of freedom statistic
dofstat <- model2$df.null - model$df.residual

# Calculate p-value using chi-square distribution
pchisq(lratiostat, df = dofstat, lower.tail = FALSE)

model3 <- glm(NObeyesdad ~ NCP + CH2O + FAF + Height + Age + TUE, family = binomial(), data = data)

# Calculate likelihood ratio statistic
lratiostat <- model3$null.deviance - model$deviance

# Calculate degrees of freedom statistic
dofstat <- model3$df.null - model$df.residual

# Calculate p-value using chi-square distribution
pchisq(lratiostat, df = dofstat, lower.tail = FALSE)

model4 <- glm(NObeyesdad ~ NCP + CH2O + FAF + Age + TUE, family = binomial(), data = data)

# Calculate likelihood ratio statistic
lratiostat <- model4$null.deviance - model$deviance

# Calculate degrees of freedom statistic
dofstat <- model4$df.null - model$df.residual

# Calculate p-value using chi-square distribution
pchisq(lratiostat, df = dofstat, lower.tail = FALSE)

model5 <- glm(NObeyesdad ~ NCP + CH2O + FAF + Height + TUE, family = binomial(), data = data)

# Calculate likelihood ratio statistic
lratiostat <- model5$null.deviance - model$deviance

# Calculate degrees of freedom statistic
dofstat <- model5$df.null - model$df.residual

# Calculate p-value using chi-square distribution
pchisq(lratiostat, df = dofstat, lower.tail = FALSE)

model6 <- glm(NObeyesdad ~ NCP + CH2O + FAF + TUE + Weight, family = binomial(), data = data)

# Calculate likelihood ratio statistic
lratiostat <- model6$null.deviance - model$deviance

# Calculate degrees of freedom statistic
dofstat <- model6$df.null - model$df.residual

# Calculate p-value using chi-square distribution
pchisq(lratiostat, df = dofstat, lower.tail = FALSE)


#Model diagnoisis
hoslem.test(data$NObeyesdad,model$fitted.values)
hoslem.test(data$NObeyesdad,model1$fitted.values)
hoslem.test(data$NObeyesdad,model2$fitted.values)
hoslem.test(data$NObeyesdad,model3$fitted.values)
hoslem.test(data$NObeyesdad,model4$fitted.values)
hoslem.test(data$NObeyesdad,model5$fitted.values)
hoslem.test(data$NObeyesdad,model6$fitted.values)

summary(model1)
exp(model1$coefficients)

# Set a threshold for classification6
cp <- 0.5

# Extract response and predicted values
response <- data$NObeyesdad
prediction <- ifelse(model6$fitted.values < cp, 0, 1)

# Evaluate model performance using confusion matrix
tab <- confusionMatrix(as.factor(prediction), as.factor(response), positive = "1")

# Plot ROC curve
par(pty = "s")
Roc <- roc(response, model6$fitted.values, plot = TRUE, legacy.axes = TRUE, print.auc = TRUE)
plot(Roc$thresholds, Roc$sensitivities, type = "l", xlab = "Cut points", ylab = "Sensitivity/Specificity", bty = "n")
lines(Roc$thresholds, Roc$specificities)
text(x = 0.7, y = 0.05, labels = "Sensitivity")
text(x = 0.7, y = 0.95, labels = "Specificity")
abline(v = 0.084, col = "gray", lty = 2)

# Change threshold for classification
cp <- 0.3

# Generate predictions using new threshold
prediction <- ifelse(model6$fitted.values < cp, 0, 1)

# Evaluate model performance using confusion matrix with new threshold
tab <- confusionMatrix(as.factor(prediction), as.factor(response), positive = "1")
tab
