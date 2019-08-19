#Author: John Hernandez
#Subject: Data Science

#PART 1: Building a simple decision tree
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

#Let's load the rpart package
library(rpart)
colnames(train)
clients_model <- rpart(Churn ~ tenure + Pago_Mensual,
           data = train,
           method = "class")
# making predictions from an rpart tree
predictions <- predict(clients_model, test, type = "class")

table(testLabel,predictions)
mean(testLabel==predictions)

#PART 2 Visualizing classification trees
clients_model
#install.packages("rpart.plot")
#now we import the library
library(rpart.plot)
rpart.plot(clients_model,type = 2)
#y otros parametros como type, digitos significativos,etc.




#PART 3: Building and evaluating a larger tree
#esta bien asi? no se debe dumificar? porque no se crearon las reglas?
loans<-read.csv("loans.csv")
loans<-loans[,c(-1,-2)]
sp<- sample(nrow(loans),0.7*nrow(loans))

##INTENTO DE DUMIFICACION------------------ NO FUNCIONO, POR QUE DA TODO IGUAL?
library(dummies)
loans <- dummy.data.frame(loans)
#-----------------------
loans_train<-loans[sp,]
loans_test<-loans[-sp,]

m <- rpart(default ~ .,
           data = loans_train,
           method = "class")
p<-predict(m,loans_test,type="class")

table(p,loans_test$default)

mean(p==loans_test$default)

rpart.plot(m,type = 2)

#PART 4: Preventing overgrown trees
#el arbol no arrojaba nada porque era muy complejo
#ahora con la profundidad maxima en 6 se obtiene un resultado distinto
customControl=rpart.control(maxdepth = 6,cp = 0)
m2 <- rpart(default ~ .,
           data = loans_train,
           method = "class",
           control = customControl)
p<-predict(m2,loans_test,type="class")

table(p,loans_test$default)
#la precision vario muy poco
mean(p==loans_test$default)
#ahora si se observan las ramificaciones en el grafico
rpart.plot(m2,type = 2)

#ahora intentamos lo mismo agregandole al customControl minsplit=70
#ahora para separar minimo deben quedar 70 elementos por hoja
customControl2=rpart.control(maxdepth = 6,cp = 0,minsplit = 70)
m3 <- rpart(default ~ .,
            data = loans_train,
            method = "class",
            control = customControl2)
p<-predict(m3,loans_test,type="class")

table(p,loans_test$default)
#la precision mejoro un poco
mean(p==loans_test$default)
#Y podemos observar que el nuevo arbol es mas visible al tener menos hojas
rpart.plot(m3,type = 2)
