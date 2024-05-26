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