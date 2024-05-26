

obesity.data <- obesity_data %>% select(Age, Height, Weight, FAF, FCVC, NCP) %>% slice(1:50)
obesity.labs <- obesity_data %>% select(NObeyesdad) %>% slice(1:50) 


sd.data <- scale(obesity.data)


data.dist <- dist(sd.data)
plot(hclust(data.dist), xlab = "", sub = "", ylab = "", main = "Complete Linkage")
