if (!require("datasets")) install.packages("datasets")
library("datasets")

if (!require("ggplot2")) install.packages("ggplot2")
library("ggplot2")

head(iris)

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

set.seed(20)

n_custers = 3
irisCluster <- kmeans(iris[, 1:4], n_custers, nstart = 20)
irisCluster

table(irisCluster$cluster, iris$Species)


irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color = irisCluster$cluster)) + geom_point()


irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color = irisCluster$cluster)) + geom_point()

