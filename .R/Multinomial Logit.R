ncaa = read.table("ncaa.txt",header=TRUE)
x = as.matrix(ncaa[4:12])
w1 = (1:16)*0 + 1
w0 = (1:16)*0
y1 = c(w1,w0,w0,w0)
y2 = c(w0,w1,w0,w0)
y3 = c(w0,w0,w1,w0)
y4 = c(w0,w0,w0,w1)
y = cbind(y1,y2,y3,y4)
library(nnet)
res =multinom(y~x)
res
res$fitted.values
rowSums(res$fitted.values)

#Truncated Variables

dnorm( 1)/(1-pnorm( 1))
-dnorm(1)/pnorm(1)