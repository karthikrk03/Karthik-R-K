# Hirerachical CLustering

library(data.table)
library(readxl)
View(EastWestAirlines)
colnames(EastWestAirlines)
ncol(EastWestAirlines)
Airlines <- EastWestAirlines[,(2:12)]
View(Airlines)

normalize_data <- scale(Airlines)
View(normalize_data)
dis_Airlines <- dist(normalize_data, method="euclidean") # distance matrix
str(dis_Airlines)

airline_clust <- hclust(dis_Airlines, method = "complete")
plot(airline_clust) # display dendrogram
plot(airline_clust, hang = -1)

group_airline <- cutree(airline_clust, k=5) # cut tree into 5 clusters
Airlines_new <- cbind(EastWestAirlines,group_airline)
setnames(Airlines_new, 'group_airline', 'group_hclust')
aggregate(Airlines_new[,(2:12)], by = list(Airlines_new$group_hclust),FUN = mean)


#k-means clustering
airlines_kmeans <- kmeans(normalize_data, 5)
str(airlines_kmeans)

airlines_kmeans$centers
Airlines_new <- cbind(Airlines_new,airlines_kmeans$cluster)
colnames(Airlines_new)
# Aggregate
aggregate(Airlines_new[,2:12],by= list(Airlines_new$`airlines_kmeans$cluster`), FUN = mean)

install.packages("cluster")
library(cluster)

xcl <- clara(normalize_data,5) #Using Centroid
clusplot(xcl)

xpm <- pam(normalize_data,5) # Using Medoids
clusplot(xpm)
