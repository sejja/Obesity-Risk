#
# HierarchicalClustering.R
# Obesity Risk
#
# Created by Diego Revilla on 07/05/24
# Copyright � 2024 . All Rights reserved
# 

library(FactoMineR)

distance_matrix <- dist(pca1$ind$coord[,1:2], method = "euclidean")
hc <-  hclust(distance_matrix, method = "complete")
plot(hc)
