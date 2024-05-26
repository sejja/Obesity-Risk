#
# HierarchicalClustering.R
# Obesity Risk
#
# Created by Diego Revilla on 07/05/24
# Copyright � 2024 . All Rights reserved
# 

library(FactoMineR)

distance_matrix <- dist(pca1$ind$coord[1:20, ], method = "euclidean")
hc.out <-  hclust(distance_matrix, method = "complete")
hc.clusters <- cutree(hc.out , length(unique(obesity_data$NObeyesdad[1:20])))
plot(hc.out)
table(hc.clusters , obesity_data$NObeyesdad[1:20])

distance_matrix <- dist(pca1$ind$coord, method = "euclidean")
hc.out <-  hclust(distance_matrix, method = "complete")
hc.clusters <- cutree(hc.out , length(unique(obesity_data$NObeyesdad)))
matrix <- table(hc.clusters , obesity_data$NObeyesdad)
colnames(matrix) <- c("Ins", "Nor", "Ob1", "Ob2", "Ob3", "Ow1", "Ow2")
plot(matrix)
