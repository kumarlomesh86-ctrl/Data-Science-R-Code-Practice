h = glm(y~x, family=binomial(link="logit"))
beta = h$coefficients
beta
dim(x)
beta=as.matrix(beta)
dim(beta)
wuns=matrix(1,64,1)
x=cbind(wuns,x)
dim(x)
xbar=as.matrix(colMeans(x))
dim(xbar)
xbar
logitfunction = exp(t(beta) %*%xbar)/(1+exp(t(beta) %*%xbar))
logitfunction
slopes = beta * logitfunction[1] * (1-logitfunction[1])
slopes
#In probit model
h = glm(y~x, family=binomial(link="probit"))
beta=h$coefficients
beta
x=as.matrix(cbind(wuns,x))
xbar=as.matrix(colMeans(x))
dim(xbar)
dim(beta)
beta=as.matrix(beta)
dim(beta)
slopes=dnorm(t(beta)%*%xbar)[1]*beta
slopes
