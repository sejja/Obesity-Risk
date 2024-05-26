library(FactoMineR)
library(tidyverse)
library(ggplot2)

############################################################################################

# K-MEANS CLUSTERING


# Perform K-means clustering
q <- 7
km.out <- kmeans(pca1$ind$coord[,1:5], q, nstart = 20)

# Create a data frame with PCA coordinates and cluster assignments
pca_data <- as.data.frame(pca1$ind$coord[,1:5])
pca_data$Cluster <- as.factor(km.out$cluster)

# Plot using ggplot2
ggplot(pca_data, aes(x = Dim.1, y = Dim.2, color = Cluster)) +
  geom_point(size = 2) +
  labs(title = paste("K-Means Clustering Results with K =", q),
       x = "Dimension 1 (28%)", y = "Dimension (21%)") +
  theme_minimal()





# Add cluster assignments to your original data
obesity_data$Cluster <- as.factor(km.out$cluster)

# Assuming NObeyesdad is an ordered factor
obesity_data$NObeyesdad <- factor(obesity_data$NObeyesdad, 
                                  levels = c("Insufficient_Weight", "Normal_Weight", "Overweight_Level_I", "Overweight_Level_II", "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"), 
                                  ordered = TRUE)

# Convert NObeyesdad to numeric for the Kruskal-Wallis test
obesity_data$NObeyesdad_numeric <- as.numeric(obesity_data$NObeyesdad)

# Perform Kruskal-Wallis test
kruskal_test <- kruskal.test(NObeyesdad_numeric ~ Cluster, data = obesity_data)
print(kruskal_test)

# Visualize the distribution of NObeyesdad across clusters
ggplot(obesity_data, aes(x = Cluster, y = NObeyesdad_numeric)) +
  geom_boxplot() +
  labs(title = "Distribution of Obesity Levels by Cluster",
       x = "Cluster", y = "Obesity Level (Numeric)") +
  theme_minimal()

ggplot(obesity_data, aes(x = Cluster, y = NObeyesdad_numeric)) +
  geom_violin() +
  labs(title = "Distribution of Obesity Levels by Cluster",
       x = "Cluster", y = "Obesity Level (Numeric)") +
  theme_minimal()