library(igraph)

el = matrix(nc=3, byrow=TRUE, c(0,1,8,0,3,4,1,3,3,3,1,1,1,2,1,1,4,7,3,4,4,2,4,1))
el

#GENERATE RANDOM GRAPH
g = erdos.renyi.game(30,0.1)
plot.igraph(g)
print(g)

#COMPUTE DEGREE DISTRIBUTION
dd = degree_distribution(g)
dd = as.matrix(dd)
d = as.matrix(seq(0,max(degree(g))))
plot (d,dd,type="l",lwd=3,col="blue",ylab="Probability",xlab="Degree")
sum(dd)

#Diameter
print(diameter(g))
res = distances(g)
res[which( res==Inf)]= 99
max(res)
length(which( res==7))
#FRAGILITY
print((t (d^2) %*% dd)/( t (d) %*% dd))

#Centrality
A = matrix(nc=3,byrow=TRUE,c(0 ,1 ,1 , 1,0,1, 1 ,1 ,0))
A

g = graph_from_adjacency_matrix(A,mode="undirected",weighted=TRUE,diag=FALSE)
res = eigen_centrality(g)
res$vector

