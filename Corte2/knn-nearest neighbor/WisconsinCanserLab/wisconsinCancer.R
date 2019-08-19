#install.packages("class")
library(class)

cancer<-read.csv("wisconsinCancer.csv")
cancer<-cancer[,-33]
set.seed(14)
sp<- sample(nrow(cancer),0.7*nrow(cancer))


train<-cancer[sp,]
test<-cancer[-sp,]
trainLabel<-train$diagnosis
testLabel<-test$diagnosis
#PD: Buscar como hacerlo con el nombre de la columna
train<-train[,-c(1,2)]
test<-test[,-c(1,2)]

dim(train)
#Realizando la compracion con ciclo for
bestk<-1
bestkPrediction<-knn(train,test,cl=trainLabel,k=bestk)
bestkAccuracy<-round(mean(bestkPrediction==testLabel),6) 

for(i in 1:40){
  #prediction and accuracy of the current k value
  prediction<-knn(train,test,cl=trainLabel,k=i)
  accuracy<-round(mean(prediction==testLabel),6)
  print(paste("k=",i," Accuracy=",accuracy*100,"%"))
  print(table(prediction,testLabel))
  
  if(bestkAccuracy<accuracy){
    bestk<-i
    bestkAccuracy<-accuracy
    } 
}

#the best k value was already calculated
print(paste("El mejor valor de k es: ",bestk,"con Precision:",bestkAccuracy*100,"%"))

