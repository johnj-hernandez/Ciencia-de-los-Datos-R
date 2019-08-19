

two_players<-matrix(data=c(0,0,9,12),ncol = 2,byrow = TRUE)

colnames(two_players)<-c("X","Y")
rownames(two_players)<-c("BLUE","RED")

dist(two_players,method = "euclidean")

?dist

two_players3<-matrix(data=c(0,0,9,12,-2,19),ncol = 2,byrow = TRUE)

colnames(two_players3)<-c("X","Y")
rownames(two_players3)<-c("BLUE","RED","GREEN")

dist(two_players3,method = "euclidean")

edit()

#---------------scaling---------
Height<- c(6,6,8)
Weight = c(200,202,200)
d<-data.frame(Height,Weight)
#d <- data.frame(list(Height = c(6,6,8), Weight = c(200,202,200))
scale(d,)

#------Dummification----
#pone como columnas los factores y para cada registro le pone 1 en la columna de la
#variable que tenia.
install.packages("dummies")
library(dummies)
color<-c("red","green","blue","blue")
sport<-c("soccer","hockey","hockey","soccer")
colorx<-data.frame(color,sport)
dum<- dummy.data.frame(colorx)
dum

