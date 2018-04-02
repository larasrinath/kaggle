library(tidyverse)
library(rpart)
library(modelr)
melbournedata<- read_csv("~/R/melb_data.csv")
attach(melbournedata)
fit<- rpart(Price ~ Rooms+ Bathroom+Landsize+YearBuilt+Lattitude+Longtitude,data = melbournedata)

plot(fit,uniform = T)
text(fit,cex=0.5)

predict(fit,head(melbournedata))
head(melbournedata$Price)

mae(model = fit, data = melbournedata)

splitdata<- resample_partition(melbournedata,c(test=0.3,train=0.7))

fit2<- rpart(Price~ Rooms+ Bathroom+Landsize+YearBuilt+Lattitude+Longtitude,data = splitdata$train)

mae(fit2, splitdata$test)
####################################
get_mae <- function(maxdepth,target,predictor,train_data,test_data){
  
  pred<- paste(predictor,collapse =  "+")
  form<- as.formula(paste(target,"~",pred,sep = ""))
  
  
  train_model<- rpart(formula =form ,data = train_data,control = rpart.control(maxdepth = maxdepth))
  
  mae<- mae(train_model,test_data)
  return(mae)
}
####################################

targets<- "Price"
predictors<- c("Rooms","Bathroom", "Landsize", "YearBuilt", "Lattitude", "Longtitude")

for (i in 1:10) {
  mae<- get_mae(maxdepth = i,target=targets,predictor = predictors,train_data = splitdata$train,test_data = splitdata$test)
  print(paste("MAPE",i,"**** mae=",mae))
  
}