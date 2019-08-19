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


#PROCEDEMOS A HACERLO CON 3 CLUSTERS con K MEANS
finalClustering=kmeans(x = datos, centers = 3)
finalClustering$
clusterAssignment2<-finalClustering$cluster

#Datos con su cluster
ecosystems <- mutate(datos, cluster =
                                 clusterAssignment2)

ggplot(ecosystems, aes(x = Colif_total, y = Colif_fecal, color =
                                   factor(cluster))) + geom_point()

#impresion modo sabio brayan
install.packages("factoextra")
library(factoextra)
fviz_cluster(finalClustering,ecosystems, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")

#METODO CON PAAAAAAAAAAAM



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
# Y tambien mostramos los centroides de cada cluster para representar a todos sus elemntos
finalClustering$centers


#CLUSTER 2
# se pudo observar que el cluster 2 cuenta con un valor mas bajo para los 3 factores microbiologicos
# dando a entender que en este se presenta un menor numero de bacterias.
# Asi mismo los valores  solidos en suspecion y contenido mineral son inferiores,pero no son los ambientes con menor numero de conductividad
# asi mismo cuenta con el valor menos variables quimicas

#CLUSTER 1
#Ambientes con un alto valor de conductividad y con mayores agentes microbiologicos que el cluster 2
#valores promedio de solidos suspendidos y de variables quimicas

#Cluster 3
#pertenecen a estos los ambientes con mas alto indice de se podria decir contaminacion
# pues hay hay un mayor numero de agentes microbianos y de solidos suspendidos.
#Asi como los factores quimios suelen ser mayores.