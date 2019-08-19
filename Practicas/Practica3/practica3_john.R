#we load the dplr library
library(dplyr)
library(ggplot2)
class(starwars)
#summary of starwars
str(starwars,vec.len =1,max.level=10)
summary(starwars)

#How many NAs distintas alternativas:
starwars %>% summarise_all(funs(sum(is.na(.))))

starwars %>% summarise_all(function(x)sum(is.na(x)))

colSums(is.na(starwars))

?summarise_all

#---------- BMI---------
colnames(starwars)
#Crear una nueva tabla llamada BMI, donde se calcule el indice de masa
#corporal (BMI) para cada uno de los personajes de starwars, además de una
#columna llamada sobrepeso, donde se coloque 1 si el BMI de la persona es mayor
#a 25 y 0 en otro caso.

#Alternativa1
starwars %>% select("name","height","mass")%>% mutate(
  bmi=mass*100/(height*height/100),
  sobrePeso=ifelse(bmi>25, 1, 0)
  )

#Alternativa2
starwars %>% select("name","height","mass")%>% mutate(
  bmi=mass*100/(height*height/100),
  sobrePeso=as.numeric(bmi>25)
)

?mutate

#-----------
#4. Crear una tabla para cada especie donde exista una columna llamada 'total'
#con el numero de personajes por especie y una median_w con la mediana de su
#peso. Ordenar en forma descendente por 'total'
x<-starwars %>% group_by(species)%>%summarize(
  median_w=median(mass,na.rm=TRUE)
  )

y<-starwars%>%group_by(species)%>%tally()

colnames(y)<-c("species","total")
speciesTable<-cbind(x,y[,"total"])
speciesTable %>%arrange(desc(total))


#5.Crear una tabla con el promedio de estatura para los diferentes personajes
#por su color de ojos, llamarla mean_h, ordenarla

starwars%>%group_by(eye_color)%>%summarise(
  mean_h=mean(height,na.rm = TRUE)
)


#6. Realizar una grafica donde se pueda analizar la piel, el color de los ojos,
#estatura y masa. (Elegir la gráfica que según su criterio genera la mayor cantidad
#de información)
ggplot(starwars, aes(x = log(mass), y = log(height),color=skin_color)) +
  geom_point() +facet_wrap(~ eye_color)



