library(graphics)
x<- rbind(matrix(rnorm(100, sd = 0.3) , ncol = 2),
         + matrix(rnorm(100, mean= 1, sd = 0.3) , ncol = 2))
colnames(x) < c("x" , "y")
## random starts do help here with too many clusters
(cl <- kmeans(x, 5, nstart = 25))
plot(x, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)
