library(faraway)
library(arules)
library(ResourceSelection)

data <- obesity_data
data$NObeyesdad <- ifelse(data$NObeyesdad == 'Overweight_Level_III', 1, 0)
data <- data[complete.cases(data),]
model <- glm(SCC~Height+Age+FCVC+FAF,family=binomial(),data=data)
lratiostat <- model$null.deviance-model$deviance
dofstat <- model$df.null- model$df.residual
pchisq(lratiostat,df=dofstat,lower.tail = FALSE)
