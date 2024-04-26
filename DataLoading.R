#
# DataLoading.R
# Obesity Risk
#
# Created by Diego Revilla on 26/04/24
# Copyright � 2024 . All Rights reserved
# 

# Function to load the obesity dataset
obesity_data_loading <- function(path) {
  data <- read.csv(path)
  
  factorized_columns <- c('Gender', 'CAEC', 'CALC', 'MTRANS', 'NObeyesdad')
  booleanized_columns <- c('FAVC', 'family_history_with_overweight', 'SMOKE', 'SCC')

  # Factorize columns
  lapply(factorized_columns, function(x) {data[[x]] <<- factor(data[[x]])})  
  
  # Factorize columns
  lapply(booleanized_columns, function(x) {data[[x]] <<- data[[x]] == 'yes'})  
  
  return(data)  
}

obesity_data <- obesity_data_loading('Datasets/ObesityDataSet.csv')




t1 <- table(obesity_data$Gender , obesity_data$NObeyesdad)
t1
# Convert the table to a data frame
count_df <- as.data.frame.table(t1)

# Plot using ggplot2
ggplot(count_df, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count of Overweight Categories by Gender",
       x = "Gender", y = "Count") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal() +
  theme(legend.position = "top")

table(obesity_data$NObeyesdad)
