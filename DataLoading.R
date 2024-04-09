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