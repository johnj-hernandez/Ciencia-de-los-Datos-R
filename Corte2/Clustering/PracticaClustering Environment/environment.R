#importamos los datos
datos<-read.table("ecosystems.txt",header=TRUE)

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
dend_colored <- color_branches(dend_ecosystems, h = 25)
plot(dend_colored)



#ahora procedemos a cortar el dendograma a la altura en la cual nos proporciona los n clusteres
cluster_assignments <- cutree(hc_ecosystems, k = 3)

library(dplyr)
ecosystems_clustered <- mutate(datos, cluster =
                              cluster_assignments)

library(ggplot2)
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
#sin embargo tambien se intentara realizar 
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)

