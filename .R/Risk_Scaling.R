#Risk Scaling
#Simulation of Effect of Increased Connectivity
#Random Graphs
library(igraph)
n=50; k=100; pvec=seq(0.05,0.50,0.05);
svec=NULL; sbarvec=NULL
for (p in pvec){
  s_temp =NULL
  sbar_temp =NULL
  for ( j in 1:k) {
    g = erdos.renyi.game(n,p,directed=TRUE);
    A= get.adjacency(g)
    diag(A) = 1
    c = as.matrix(round(runif(n,0,2) ,0))
    syscore = as.numeric(sqrt(t(c) %*%A%*%c))
    sbarscore = syscore/n
    s_temp = c(s_temp,syscore)
    sbar_temp = c(sbar_temp,sbarscore)
  }
  svec = c(svec,mean(s_temp))
  sbarvec = c(sbarvec ,mean(sbar_temp))
}

plot(pvec,svec,type="l",
     xlab="Prob of connecting to a node",
     ylab="S",lwd=3,col="red")
plot(pvec,sbarvec,type="l",
     xlab="Prob of connecting to a node",
     ylab="S_Avg",lwd=3,col="red")