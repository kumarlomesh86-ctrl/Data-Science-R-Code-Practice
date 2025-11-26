#TOO BIG TO FAIL
#SIMULATION OF EFFECT OF INCREASED NODES AND REDUCED CONNECTIVITY
nvec=seq(10,100,10); k=5000; svec=NULL; sbarvec=NULL
for (n in nvec) {
  s_temp =NULL
  sbar_temp =NULL
  p= 5/n
  for ( j in 1:k) {
    g = erdos.renyi.game(n,p,directed=TRUE);
    A= get.adjacency(g)
    diag(A) = 1
    c = as.matrix(round(runif(n,0,2),0))
    syscore = as.numeric(sqrt(t(c) %*%A%*%c))
    sbarscore = syscore/n
    s_temp = c(s_temp,syscore)
    sbar_temp = c(sbar_temp,sbarscore)
  }
  svec = c(svec,mean(s_temp))
  sbarvec = c(sbarvec,mean(sbar_temp))
}
plot(nvec,svec,type="l" ,
     xlab="Number of nodes",ylab="S" ,
     ylim=c(0,max(svec)),lwd=3,col="red")
plot(nvec,sbarvec,type="l" ,
     xlab="Number of nodes",ylab="S_Avg" ,
     ylim=c(0,max(sbarvec)),lwd=3,col="red")