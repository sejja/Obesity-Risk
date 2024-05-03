#
# PCA.R
# Obesity Risk
#
# Created by Diego Revilla on 26/04/24
# Copyright � 2024 . All Rights reserved
# 

library(FactoMineR)
library(plotrix)

#Choose the six variables to do Principal Component Analysis
pca_dataset <- obesity_data %>% select(Age, Height, Weight, FAF, FCVC, NCP)

#Normalize the dataset
pca_dataset <- scale(pca_dataset)

#multiply the dataset data to match the corrected standar deviation with the uncorrected standard deviation
#, with a difference of sqrt(5/6)
pca_dataset <- pca_dataset * sqrt(5/6)

#calculate the correlation matrix
correlation_matrix <- cor(pca_dataset)

#compute the eigenvectors of the correlation matrix
eigen_vec <- eigen(correlation_matrix)

# The relative variance associated to each eigenvector is the eigenvalue divided by the sum of eigenvalues
variance_table <- cbind(eigen_vec$values,eigen_vec$values/sum(eigen_vec$values),
             cumsum(eigen_vec$values/sum(eigen_vec$values)))

#We note that the variable that contributes the less is the lst one, with only a 7% of description of the total variance

#Compute the coordinates for all variables
coordinates <- t(solve(eigen_vec$vectors)%*%t(pca_dataset))

# Calculate correlations between coordinates and each column of pca_dataset
correlations <- sapply(1:ncol(pca_dataset), function(j) {
  c(cor(coordinates[, 1], pca_dataset[, j]), cor(coordinates[, 2], pca_dataset[, j]))
})

# Convert correlations to a matrix and assign column names
correlations <- as.data.frame(t(correlations))
colnames(correlations) <- c("Correlation_Coord1", "Correlation_Coord2")

# Assign row names to correlations
rownames(correlations) <- colnames(pca_dataset)

# Given that the data is standardized, the center of the cloud is the zero vector.
# Therefore the distance to the center of every individual is just the modulus of the vector
sqrt(apply(pca_dataset^2,MARGIN=1,FUN=sum))

# Influential #
# Influential individuals or variables can be detected by computing their contribution to the coordinates and correlations
# Individual contribution
indcontr1 <- (coordinates[,1]^2)/sum(coordinates[,1]^2)
indcontr2 <- (coordinates[,2]^2)/sum(coordinates[,2]^2)

# Variable contribution
varcontr1 <- (correlations[,1]^2)/sum(correlations[,1]^2)
varcontr2 <- (correlations[,2]^2)/sum(correlations[,2]^2)

pca1 <- PCA(pca_dataset,scale.unit = TRUE)
variable_correlation <- (pca1$var$cor)^2
