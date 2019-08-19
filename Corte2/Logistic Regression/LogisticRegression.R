#Por medio de la regresion logistica podremos predecir entre decisiones binarias la pertenencia
#de un valor , me devuelve 0 o 1 , usando la funcion sigmoide la cual me da valores entre 0 a 1
#luego por medio de un umbral vamos a definir cuales son 0 y 1

#leemos los datos
donors<- read.csv('donors.csv')
donors
str(donors)


#realizamos el modelo de regresion logistica relacionando 3 variables y family binomial para 0 y 1
donation_model <-
  glm(donated ~ bad_address + interest_religion
      + interest_veterans,
      data = donors, family = "binomial")


donation_model

#vemos cuales son los coeficientes de la ecuacion 
donation_model$coefficients


#vemos el resumen del modelo en donde podemos ver la significancia de estos
summary(donation_model)

#ahora vemos cuantos donaron realmente (reales)
table(donors$donated)

#en los datos de donors le agregamos una matriz con las probabilidades de que donden
#arrojadas por la prediccion y el modelo de regresion lineal
donors$donation_prob <-
  predict(donation_model, type = "response")


donors$donation_prob
colnames(donors)

#calculamos la proporcion de los datos reales y con esto vamos a probar si las probabildiades
#calculadas fueron adecuadas
mean(donors$donated)

donors$donation_pred <- ifelse(donors$donation_prob > 0.05040551,1,0)
       donors$donation_pred
 mean(donors$donated == donors$donation_pred)                               
      
 
 #probabilidad de que no donen y donen
 mean(donors$donated == 0)                               
 mean(donors$donated == 1) 

 #preguntar porque igual  a 0 
#matriz de confusion
table(x= donors$donated,y = donors$donation_pred)

table(donors$donated,rep(0,length(donors$donated)))


install.packages("pROC")     
library(pROC)
ROC <- roc(donors$donated, donors$donation_prob)
ROC
auc(ROC)
plot(ROC, col = "blue")

str(donors)
# * son interacciones
#tenemos el modelo con la nueva combinacion de variables
rfm_model <- glm(donated ~ money + recency*frequency, donors, family = "binomial")
#Vemos las probabilidades
rfm_prob <- predict(rfm_model, type = "response")
summary(rfm_model)
#realizamos las predicciones con el nuevo modelo
rfm_pred<- ifelse(rfm_prob>0.05040551,1,0)

#la precision con el nuevo modelo disminuyo sin embargo hay que verificar si la sensitividad mejoro
#sensitividad aumento, primero era 0.19 y luego 0.555 osea mejor
#osea va a darme mas valores positivos aun si se equivoca con los negativos
mean(donors$donated==rfm_pred)


#grafico inicial de roc
ROC<- roc(donors$donated, donors$donation_prob)
plot(ROC,col="blue",main="Grafico inicial")

#grafico con nuevo modelo 
ROC2<- roc(donors$donated, rfm_prob)
plot(ROC2,col="red",main="Grafico con modelo rfm")



