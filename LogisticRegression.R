library(faraway)
library(arules)
library(ResourceSelection)
library(caret)
library(pROC)

data <- obesity_data
data$NObeyesdad <- ifelse(data$NObeyesdad == 'Obesity_Type_III' | data$NObeyesdad == 'Obesity_Type_II', 1, 0)
data <- data[complete.cases(data),]
model <- glm(NObeyesdad~NCP+CH2O+FAF+TUE,family=binomial(),data=data)
lratiostat <- model$null.deviance-model$deviance
dofstat <- model$df.null- model$df.residual
pchisq(lratiostat,df=dofstat,lower.tail = FALSE)
cp<-0.5
response <- data$NObeyesdad
prediction <- ifelse(model$fitted.values < cp,0,1) 
tab<-confusionMatrix(as.factor(prediction),as.factor(response),positive="1")
par(pty="s")
Roc <- roc(response,model$fitted.values,plot=TRUE,legacy.axes=TRUE,print.auc=TRUE)
plot(Roc$thresholds,Roc$sensitivities,type="l",xlab="Cut points",ylab="Sensitivity/Specificity",bty="n")
lines(Roc$thresholds,Roc$specificities)
text(x=0.7,y=0.05,labels = "Sensitivity")
text(x=0.7,y=0.95,labels = "Specificity")
abline(v=0.084,col="gray",lty=2)
cp<-0.32
prediction <- ifelse(model$fitted.values < cp,0,1) 
tab<-confusionMatrix(as.factor(prediction),as.factor(response),positive="1")
