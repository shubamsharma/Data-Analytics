iris2 <- iris
iris2$Species <- NULL
(kmeans.result <- kmeans(iris2, 3))

iris$Species

data <- cbind(kmeans.result$cluster,iris$Species )

data

table(iris$Species, kmeans.result$cluster)

irisheaxd

plot(iris2[c("Sepal.Length", "Sepal.Width")], col = iris$Species)

plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)

points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3,pch = 8, cex=2)




idx <- sample(1:dim(iris)[1], 40)

irisSample <- iris[idx,]
irisSample$Species <- NULL
dist(irisSample)

hc <- hclust(dist(irisSample), method="ave")
plot(hc, hang = -1, labels=iris$Species[idx])
# cut tree into 3 clusters
rect.hclust(hc, k=4)
groups <- cutree(hc, k=3)

groups



