y1 = 1:32
y1 = y1*0+1
y1
y2=y1*0
y2
y=c(y1,y2)
y
ncaa <- read.table("ncaa.txt", header = TRUE)
x=as.matrix(ncaa[4:12])
library
h = glm(y~x,family=binomial(link="logit"))
logLik(h)
summary(h)
h = lm(y~x)
summary(h)
