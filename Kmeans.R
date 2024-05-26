## Clustering ##

# INTRODUCTION #
# We will use FactoMineR and tidyverse libraries
# you may have to install the second one  
library(FactoMineR)
library(tidyverse)







# K-means #   
q <- 3
km.out <- kmeans(pca1$ind$coord[,1:5], q, nstart = 20)

# We
# can plot the data, with each observation colored according to its cluster
# assignment.
par(mfrow = c(1, 1))
plot(pca1$ind$coord[,1:2], col = (km.out$cluster + 1), ylim = c(-3,2),
     main = paste("K-Means Clustering Results with K = ", q, sep = " "),
     xlab = "", ylab = "", pch = 20, cex = 2)
