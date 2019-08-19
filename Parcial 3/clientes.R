clientes<-read.csv("clientes.csv")
clientes<-na.omit(clientes)
View(clientes)

#creamos el conjunto de datos de entrenamiento y de test

set.seed(14)
sp<- sample(nrow(clientes),0.75*nrow(clientes))

train<-clientes[sp,]
test<-clientes[-sp,]
#guardamos las etiquetas
trainLabel<-train$Churn
testLabel<-test$Churn

#aHORA ELIMINAMOS EL ID Y LA COLUMNA DE ETIQUETAS YA GUARDADA
n<-ncol(clientes)
train<-train[,-c(1,n)]
test<-test[,-c(1,n)]

View(train)

#ahora procedemos a dumificar los datos
# install.packages('dummies')
library(dummies)
dummy_train <- dummy.data.frame(train)
dummy_test <- dummy.data.frame(test)


View(dummy_train) # verificar la dumificación de los datos

library(class)

#sum(is.na(dummy_train))
#summary()
prediction<-knn(dummy_train,dummy_test, cl= trainLabel)
mean(prediction==testLabel)
