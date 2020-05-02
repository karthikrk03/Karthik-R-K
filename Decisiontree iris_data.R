install.packages("party")
library(party)
str(iris)
head(iris)
set.seed(1234)
ind <- sample(2, nrow(iris), replace = TRUE, prob= c(0.7,0.3))

trainData <- iris[ind==1,]
testData <- iris[ind==2,]

Formula <- Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width
iris_ctree <- ctree(Formula, data=trainData)

train_predict <- predict(iris_ctree,trainData, type="response")
table(train_predict,trainData$Species)

mean(train_predict!= trainData$Species)*100

test_predict <- predict(iris_ctree, newdata=testData,type="response")
table(test_predict, testData$Species)

mean(test_predict!= testData$Species)*100
print(iris_ctree)
plot(iris_ctree)
plot(iris_ctree, type= "simple")
