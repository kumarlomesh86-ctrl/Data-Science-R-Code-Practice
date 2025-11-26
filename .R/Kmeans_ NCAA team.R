ncaa=read.table("ncaa.txt", header=TRUE)
names(ncaa)
fit=kmeans(ncaa[,3:12],4)
fit$size
fit$centers
