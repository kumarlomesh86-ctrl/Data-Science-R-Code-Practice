ncaa=read.table("ncaa.txt", header=TRUE)
x=ncaa[4:12]
result=princomp(x)

par(mfrow = c(1,2))
screeplot(result)
screeplot(result, type="lines")
summary(result)
result$loadings
result$sdev
biplot(result)
prcomp(x)
