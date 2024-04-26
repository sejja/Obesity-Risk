#
# PCA.R
# Obesity Risk
#
# Created by Diego Revilla on 26/04/24
# Copyright � 2024 . All Rights reserved
# 

#Choose the six variables to do Principal Component Analysis
pca_dataset <- obesity_data %>% select(Age, Height, Weight, FAF, FCVC, NCP)