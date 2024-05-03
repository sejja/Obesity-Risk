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

plot(coordinates[,1],coordinates[,2],xlab="Component 1 (27,89%)",ylab="Component 2 (21,78%)",
     xlim=c(-4,4),ylim=c(-2.5,2.5),pch=16)
text(coordinates[,1],coordinates[,2]+0.4,labels = row.names(pca_dataset),cex=1)
abline(h=0,v=0,lty=2,col="lightgray")


# Calculate correlations between coordinates and each column of pca_dataset
correlations <- sapply(1:ncol(pca_dataset), function(j) {
  c(cor(coordinates[, 1], pca_dataset[, j]), cor(coordinates[, 2], pca_dataset[, j]))
})

# Convert correlations to a matrix and assign column names
correlations <- as.data.frame(t(correlations))
colnames(correlations) <- c("Correlation_Coord1", "Correlation_Coord2")

# Assign row names to correlations
rownames(correlations) <- colnames(pca_dataset)

plot(correlations,xlim=c(-1,1),ylim=c(-1,1),pch=16,cex=0.3,
     xlab="Component 1 (27,89%)",ylab="Component 2 (21,78%)",asp=1)
draw.circle(0,0,radius=1)
arrows(x0 = 0,y0 = 0,x1 = correlations[,1],y1=correlations[,2],length=0.1)
abline(h=0,v=0,lty=2)
text(correlations[,1]+0.1,correlations[,2]+0.1,labels = row.names(correlations),cex=1)

# Overlay the scatter plot of individual data points
plot(correlations, xlim = c(-3, 3), ylim = c(-2, 2), pch = 16, cex = 0.3,
     xlab = "Component 1 (67.77%)", ylab = "Component 2 (19.05%)", asp = 1)
points(coordinates[, 1], coordinates[, 2], pch = 16, col = "lightgray")
text(coordinates[, 1], coordinates[, 2] + 0.4, labels = row.names(pca_dataset), cex = 0.8)
abline(h = 0, v = 0, lty = 2, col = "lightgray")

# Create the scatter plot with vectors
draw.circle(0, 0, radius = 1)
arrows(x0 = 0, y0 = 0, x1 = correlations[, 1], y1 = correlations[, 2], length = 0.1)
abline(h = 0, v = 0, lty = 2)
text(correlations[, 1] + 0.1, correlations[, 2] + 0.1, labels = row.names(correlations), cex = 0.6)

# Optionally, add a title
title(main = "PCA Analysis: Individuals and Vectors")

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

plot(variable_correlation,xlim=c(-1,1),ylim=c(-1,1),pch=16,cex=0.3,
     xlab="Component 1 (27,89%)",ylab="Component 2 (21,78%)",asp=1)
draw.circle(0,0,radius=1)
arrows(x0 = 0,y0 = 0,x1 = variable_correlation[,1],y1=variable_correlation[,2],length=0.1)
abline(h=0,v=0,lty=2)
text(variable_correlation[,1]+0.1,variable_correlation[,2]+0.1,labels = row.names(variable_correlation),cex=1)
title(main = "PCA Analysis: PCA Final Analysis")
