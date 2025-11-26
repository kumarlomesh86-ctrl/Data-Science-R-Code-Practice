#FUNCTION TO COMPUTE FULL RETURN DISTRIBUTION
#INTEGRATES OVER X BY CALLING ASBREC
digiprob = function(L,q,rho) {
  dx = 0.1
  x = seq( 40,40)*dx
  fx = dnorm(x)*dx
  fx = fx/sum(fx)
  maxloss = sum(L)
  bucket = c(0,seq(maxloss))
  totp = array(0,(maxloss+1))
  for (i in seq(length(x))) {
    p=pnorm((qnorm(q)-rho*x[i ]) /sqrt(1-rho^2))
    ldist = asbrec(L,p)
    totp = totp + ldist[ ,2]*fx[i]
  }
  result =matrix(c(bucket, totp),(maxloss+1),2)
}

# INTEGRATE OVER CONDITIONAL DISTRIBUTIONS
w= c(5,8,4,2,1)
q= c(0.1,0.2,0.1,0.05,0.15)
rho = 0.25
res1 = digiprob(w,q,rho)
rho = 0.75
res2 = digiprob(w,q,rho)
par(mar=c(5,4,2,1))
barplot(res1[,2],names.arg=res1[,1], xlab="portfolio value",
        ylab="probability")
barplot(res2[,2],names.arg=res2[,1], xlab="portfolio value",
        ylab="probability")
  

cbind(res1, res2)