data = read.csv("vc_clust.csv",header=TRUE,sep=",")
dim(data)
names(data)
idx=which(rowSums(is.na(data))==0)
length(idx)
data=data[idx,]
length(idx)
idx=c(3,6,31,32)
cdata=data[,idx]
dim(data)
names(cdata)
fit=kmeans(cdata,4)
fit$size
fit$centers

#for four cluster
idx=c(25,26,27,28,29,30,31,32)
cdata=data[,idx]
names(cdata)
fit=kmeans(cdata,4)
fit$size
fit$centers

#Now,assuming6clusters,we have
fit=kmeans(cdata,6)
fit$size
fit$centers
