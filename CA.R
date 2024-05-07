library(dplyr)
library(knitr)
library(ca)


# Construct a contingency table 
obesity_table <- table(obesity_data$CAEC, obesity_data$NObeyesdad) %>% as.data.frame.array() %>% 
  select(Insufficient_Weight,Normal_Weight,Overweight_Level_I,Overweight_Level_II,Obesity_Type_I,Obesity_Type_II,Obesity_Type_III)

kable(obesity_table)

# Calculate probability table
prob_table <- obesity_table/sum(obesity_table)
kable(prob_table)


# Perform Chi-Square test
xtest <- chisq.test(obesity_table)
xtest

# Calculate the total number of observations
total_observations <- sum(obesity_table)


# Get expected values
kable(round(xtest$expected, 0))
kable(round(xtest$expected,0) / total_observations)



# Is the difference big enough?
X2 <- sum(xtest$residuals^2)

# Let's compute the pvalue
xtest$p.value

# Relative contribution of each cell
(xtest$residuals^2)/xtest$statistic



# Perfrom eigendecomposition 

M <- xtest$residuals/sqrt(total_observations) 
M <- M%*%t(M)
eig <- eigen(M)


cumulative <- cumsum(eig$values)/sum(eig$values)



# Create a plot for explained variance
plot(1:length(eig$values), eig$values,  ylim = c(0, 1.1), type = "b", pch = 19, col = "darkblue", 
     xlab = "Component", ylab = "Explained Variance",
     main = "Explained Variance by Component")

lines(1:length(eig$values), cumulative, type = "b", pch = 19, col = "red")
legend("center", legend = c("Cumulative Variance","Explained Variance" ),col = c("red", "darkblue"), lty = 1, pch = 19)




# Let's perform ca
ca_table <- ca(obesity_table)

# Let's visualize it
plot(ca_table,invisible="row")

# The main indicators can be obtained with
summary(ca_table)


