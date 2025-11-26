library(igraph)
A=matrix(c(0,6,5,0,0,6,0,2,0,0,5,2,0,2,0,0,0,2,0,10,0,0,0,10,0),5,5)
g = graph.adjacency(A,mode="undirected",diag=FALSE)
wtc =cluster_walktrap(g)
res=cut_at(wtc,no=3)
res
print(res)

#MODULARITY
Amodularity = function(A, delta ) {
  n = length(A[1 ,])
  d = matrix(0,n,1)
  for ( j in 1:n) { d[ j ] = sum(A[j,]) }
  m = 0.5*sum(d)
  Q = 0
  for ( i in 1:n) {
    for ( j in 1:n) {
      Q = Q + (A[i,j]-d[i]*d[ j ]/(2*m))*delta[i,j]
    }
  }
  Q = Q/(2*m)
}

A = matrix(c(0 ,6 ,5 ,0 ,0 ,6 ,0 ,2 ,0 ,0 ,5 ,2 ,0 ,2 ,0 ,0 ,0 ,2 ,0 ,10 ,0 ,0 ,0 ,10 ,0) ,5 ,5)
delta = matrix(c(1 ,1 ,1 ,0 ,0 ,1 ,1 ,1 ,0 ,0 ,1 ,1 ,1 ,0 ,0 ,0 ,0 ,0 ,1 ,1 ,0 ,0 ,0 ,1 ,1) ,5 ,5)
print(Amodularity(A, delta ))

g = graph.adjacency(A,mode="undirected",weighted=TRUE,diag=FALSE)

wtc=walktrap.community(g,modularity=TRUE,weights=E(g)$weight)
res=cut_at(wtc,no=3)
print(res)
print(modularity(g,res$membership,weights=E(g)$weight))
