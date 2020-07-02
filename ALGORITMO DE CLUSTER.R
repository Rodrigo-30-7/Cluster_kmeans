#########################################################################################
#########################################################################################
################################ MODELO DE CLUSTER ######################################
#########################################################################################
#########################################################################################
library(data.table)

setwd("C:/Users/FelipeHernandez/Desktop/Predictable Media/Cluster")

dataset=fread("KMeans.csv", sep=";", header = T)

head(tabla)
names(tabla)
str(tabla)
library(ggplot2)

ggplot() + geom_point(aes(x = X, y = Y), data = dataset, alpha = 0.5) + ggtitle('Conjunto de Datos')

set.seed(1234)
wcss <- vector()
for(i in 1:20){
  wcss[i] <- sum(kmeans(dataset, i)$withinss)
}

ggplot() + geom_point(aes(x = 1:20, y = wcss), color = 'blue') + 
  geom_line(aes(x = 1:20, y = wcss), color = 'blue') + 
  ggtitle("Método del Codo") + 
  xlab('Cantidad de Centroides k') + 
  ylab('WCSS')

set.seed(1234)

##########################################################
###################### SIMULACIÓN ########################
##########################################################

kmeans <- kmeans(dataset, 9, iter.max = 1000, nstart = 10)

dataset$cluster <- kmeans$cluster
ggplot() + geom_point(aes(x = X, y = Y, color = cluster), data = dataset, size = 2) +
  scale_colour_gradientn(colours=rainbow(4)) +
  geom_point(aes(x = kmeans$centers[, 1], y = kmeans$centers[, 2]), color = 'black', size = 3) + 
  ggtitle('Clusters de Datos con k = 7 / K-Medios') + 
  xlab('X') + ylab('Y')

set.seed(1234)
kmeans <- kmeans(dataset, 8, iter.max = 1000, nstart = 10)
dataset$cluster <- kmeans$cluster
ggplot() + geom_point(aes(x = X, y = Y, color = cluster), data = dataset, size = 2) +
  scale_colour_gradientn(colours=rainbow(4)) +
  geom_point(aes(x = kmeans$centers[, 1], y = kmeans$centers[, 2]), color = 'black', size = 3) + 
  ggtitle('Clusters de Datos con k = 8 / K-Medios') + 
  xlab('X') + ylab('Y')

write.csv(kmeans, file="K-Medias.csv")

