library(ggplot2);library(Metrics);library(caret);library(mlbench);library(caTools)
library(C50)
model <- lm(price ~ ., diamonds)
p <- predict(model, diamonds)
#Compute errors using the formula errors = predicted ??? actual. Save the result to error.
errors=p-diamonds$price
#Compute RMSE
errors=errors^2
errors=sum(errors)
errors=sqrt(errors/length(p))
rmse(diamonds$price,p)

#Cross-validation approaches

model <- train(
  price ~ .,diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)

model #RMSE 1130.489

#now with repeated

model <- train(
  price ~ .,diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 5,
    repeats = 5, verboseIter = TRUE
  )
)

model #RMSE 1131.239


data(Sonar)

sp<- sample(nrow(Sonar),0.6*nrow(Sonar))

train<-Sonar[sp,]
test<-Sonar[-sp,]

model<-glm(Class~.,family="binomial", train)

p<-predict(model,test,type="response")

m_or_r <- ifelse(p > 0.5, "M", "R")
p_class <- factor(m_or_r, levels = levels(test$Class))
confusionMatrix(p_class, test$Class)

m_or_r <- ifelse(p > 0.9, "M", "R")
p_class <- factor(m_or_r, levels = levels(test$Class))
confusionMatrix(p_class, test$Class)

m_or_r <- ifelse(p > 0.1, "M", "R")
p_class <- factor(m_or_r, levels = levels(test$Class))
confusionMatrix(p_class, test$Class)





colAUC(p,test$Class, plotROC = TRUE)

myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE
)

model<-train(method="glm",data=Sonar,Class~.,trControl=myControl)
model
#data(churn_y)
data(churn)
myFolds <- createFolds(churn_y, k = 5)
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, 
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)

modelGMLN <- train(
  x = churn_x, y = churn_y,
  metric = "ROC",
  method = "glmnet",
  trControl = myControl
)

ModelRandomForest <- train(
  x = churn_x, y = churn_y,
  metric = "ROC",
  method = "ranger",
  trControl = myControl
)

model_list <- list(item1 = modelGMLN, item2 = ModelRandomForest)
resamples<-resamples(model_list)
summary(resamples)
bwplot(resamples, metric = "ROC")
xyplot(resamples,metric="ROC")



