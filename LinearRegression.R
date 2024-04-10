# Perform linear regression
linear_regression_model <- lm(Weight ~ Age + Height + family_history_with_overweight, data = obesity_data)

# Summarize the regression results
summary(linear_regression_model)

plot(obesity_data$Weight, fitted(linear_regression_model), main = "Actual vs. Predicted", xlab = "Actual Weight", ylab = "Predicted Weight")
abline(0, 1, col = "red")  # Add a diagonal line for perfect prediction
