

data_lm <- obesity_data


# Perform linear regression
linear_regression_model <- lm(Weight ~ Height + family_history_with_overweight + FCVC + FAF ,data = data_lm)

# Calculate the confident interval
confint((linear_regression_model))

# Summarize the regression results
anova(linear_regression_model)

# Plot the predicted weight against the actual weight
plot(obesity_data$Weight, fitted(linear_regression_model), main = "Actual vs. Predicted", xlab = "Actual Weight", ylab = "Predicted Weight")
abline(0, 1, col = "red",)  # Add a diagonal line for perfect prediction


# Let's check the heteroscedasticity
plot(linear_regression_model,1)

# Let's check the influential points with cooks distance
plot(linear_regression_model,4)

# Interpretation
summary(linear_regression_model)