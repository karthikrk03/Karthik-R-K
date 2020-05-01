install.packages("plyr")
library(plyr)
install.packages("animation")
library(animation)
install.packages("kselection")
library(kselection)

crime_na <- na.omit(USArrests)
crime <- data.matrix(crime_na)
str(crime)

fit <- kmeans(crime,5)
class(fit)
str(fit)

View(crime)
mydata <- crime[1:50,c(1:4)]
View(mydata)
normalized_data <- scale(mydata[,1:4])
View(normalized_data)
fit <- kmeans(normalized_data,5)
str(fit)
final2 <- data.frame(crime, fit$cluster)
View(final2)
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
View(final3)
aggregate(mydata[,1:4],by=list(fit$cluster), FUN = mean)

#elbow curve & k ~ sqrt(n/2) to decide the k value

wss=(nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))
for (i in 1:5) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:5, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")


fit <- kmeans.ani(crime, 4)

fit$centers
