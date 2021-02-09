library(caret)
library(tidyverse)
library(xgboost)
library(MLeval)
library(xtable)
library(reporttools)
library(rpart)
library(rpart.plot)
library(devtools) 
library(xgboost)
library(SHAPforxgboost)

draw_confusion_matrix <- function(cm) {
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, '無事故', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, '發生事故', cex=1.2)
  text(125, 370, '預測label', cex=1.3, srt=90, font=2)
  text(245, 450, '真實label', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, '無事故', cex=1.2, srt=90)
  text(140, 335, '發生事故', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "統計量", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(35, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(35, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(65, 85, names(cm$byClass[3]), cex=1.2, font=2)
  text(65, 70, round(as.numeric(cm$byClass[3]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[4]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[4]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(50, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(50, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
} 


load("C:/Users/User/Desktop/freeway/arrange5_tot_1.RData")
pred = predict(xgb.fit_acc, val)
confusionMatrix(data = pred, reference = as.factor(val$counts))
draw_confusion_matrix(confusionMatrix(data = pred, reference = as.factor(val$counts)))
xgbImp = varImp(xgb.fit_acc, scale = TRUE)
plot(xgbImp, main = "前十大關鍵變數", top = 10)


pred = predict(xgb.fit_spec, val)
confusionMatrix(data = pred, reference = as.factor(val$counts))
draw_confusion_matrix(confusionMatrix(data = pred, reference = as.factor(val$counts)))
xgbImp = varImp(xgb.fit_spec, scale = TRUE)
plot(xgbImp, main = "前十大關鍵變數", top = 10)





shap_values <- shap.values(xgb_model = m1, X_train = train[,c(2:31,33:42)])
shap_long <- shap.prep(xgb_model = m1, X_train = train[,c(2:31,33:42)])
shap.plot.summary(shap_long)


tree.fit = rpart(counts~.,data = train)
rpart.plot(tree.fit)
