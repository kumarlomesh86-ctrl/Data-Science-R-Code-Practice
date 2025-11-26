library(grid)
library(MASS)
library(neuralnet)

names(infert)
summary(infert)

# Let’s fit a logit model
res = glm(case ~ age+parity+induced+spontaneous,family=binomial(link="logit"), data=infert)
summary(res)

nn = neuralnet(case~age+parity+induced+spontaneous,hidden=2,data=infert,err.fct = "ce",linear.output = FALSE,likelihood = TRUE) 
nn
head(cbind(nn$covariate,nn$net.result[[1]]))
cor(cbind(nn$net.result[[1]],res$fitted.values))
nn2 = neuralnet(case~age+parity+induced+spontaneous,hidden=2,algorithm="rprop+",data=infert)
cor(cbind(nn2$net.result[[1]],res$fitted.values))
cor(cbind(nn2$net.result[[1]],nn$fitted.result[[1]]))
compute(nn,covariate=matrix(c(30,1,0,1),1,4))

confidence.interval(nn,alpha=0.10)
confidence.interval(nn,alpha=0.95)
