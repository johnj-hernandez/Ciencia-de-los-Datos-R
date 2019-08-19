#importamos los datos
datos<-read.table("ecosystems.txt",header=TRUE)

#importamos las librerias;
library(dplyr)
library(ggplot2)

#Como podemos ver tenemos 57 observaciones cada una con 7 factores diferentes, que utilizaremos para identificar
#los distintos grupos en los que podemos asociar las distintas localizaciones

#encontramos las matrices de interdistancias
distancias<-dist(datos)


#-----------------------ANALISIS POR CLUSTER HERARQUICO-----------------

#creamos el objeto cluster usando el metodo complete
hc_ecosystems <- hclust(distancias, method = 'complete')


#el dendograma nos permitira ver la formacion de los grupos a medida de que la altura de sus distancias
#aumenta ELABORAR
library(dendextend)
dend_ecosystems <- as.dendrogram(hc_ecosystems)
dend_colored <- color_branches(dend_ecosystems, h = 3)
plot(dend_colored)



#ahora procedemos a cortar el dendograma a la altura en la cual nos proporciona los 3 clusteres
#(Este numero 3 fue seleccionado por medio del diagrama de codo en la seccion siguiente)
cluster_assignments <- cutree(hc_ecosystems, k = 3)


ecosystems_clustered <- mutate(datos, cluster =
                              cluster_assignments)

ggplot(ecosystems_clustered, aes(x = Colif_total, y = Colif_fecal, color =
                                factor(cluster))) +
  geom_point()

#-------------------------ANALISIS POR CLUSTER NO JERARQUICO------------------------------------
#haremos uso del metodo k means para el analisis por clutering no jerarquico
library(purrr)
#graficamos el diagrama de codo con hasta 10 clusteres para ver que tanto varian para estos numeros
#e identificar el numero correcto de clusteres
tot_withinss <- map_dbl(1:10, function(k){
  model <- kmeans(x = datos, centers = k)
  model$tot.withinss
})


elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)
elbow_df

##una ves tenemos los valores para cada k procedemos a hacer la grafica de codo
#se puede observar por la grafica que convendria caracterizar los datos con entre 3 hasta 5 clusteres
#Se intentara realizar con 3 clusters ya que a partir de aqui la varianza es muy poca
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)


#PROCEDEMOS A HACERLO CON 3 CLUSTERS
finalClustering=kmeans(x = datos, centers = 3)
clusterAssignment2<-finalClustering$cluster

#Datos con su cluster
ecosystems <- mutate(datos, cluster =
                                 clusterAssignment2)

ggplot(ecosystems, aes(x = Colif_total, y = Colif_fecal, color =
                                   factor(cluster))) + geom_point()

#-----------------------------------------------------------
# A partir de estos puntos se continuara trabajando con los resultados obtenidos por
#la clasificacion por k means

#los datos de los ecosistemas estan asociados con el cluster al que pertenecen en la variable ecosystems
ecosystems

#A continuacion intentaremos identificar las caracteristicas de cada cluster

#numero de ecosistemas que caen en cada cluster
count(ecosystems, cluster)

#Ahora calculamos los valores promedio de todos los factores ambientales para cada cluster
averages<-ecosystems %>% group_by(cluster) %>% summarise_all(funs(mean(.)))
averages
