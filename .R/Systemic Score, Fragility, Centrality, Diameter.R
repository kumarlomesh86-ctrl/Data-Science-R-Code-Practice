library(igraph)
#FUNCTION FOR RISK INCREMENT AND DECOMP
NetRisk = function(Ri ,X) {
  S = sqrt(t(Ri) %*%X%*%Ri)
  RiskIncr = 0.5 * (X%*%Ri + t(X) %*%Ri)/S[1,1]
  RiskDecomp = RiskIncr * Ri
  result = list(S,RiskIncr ,RiskDecomp)
}

#we generate a network of 15 banks by creating a random graph.
#CREATE ADJ MATRIX
e = floor(runif(15*15)*2)
X=matrix(e,15,15)
diag(X) = 1
#GRAPH NETWORK: plot of the assets and the links with directed arrow
na = length(diag(X))
Y=X; diag(Y)=0
g = graph.adjacency(Y)
plot.igraph(g,layout=layout.fruchterman.reingold,
             edge.arrow.size=0.5,vertex.size=15,
             vertex.label=seq(1,na))
#CREATE CREDIT SCORES
Ri =matrix(floor(runif(na)*4),na,1)
Ri
#COMPUTE OVER ALL RISK SCORE AND RISK INCREMENT
res =NetRisk(Ri,X)
S = res[[1]]; print(c("Risk Score" ,S))
RiskIncr = res[[2]]
               
#NETWORK FRAGILITY
deg = rowSums(X)-1
frag =mean(deg^2)/mean(deg)
print(c("Fragility score = ",frag))

#NODE EIGEN VALUE CENTRALITY
cent = evcent(g)$vector
print("Normalized Centrality Scores")
print(cent)
sorted_cent = sort(cent,decreasing=TRUE,index.return=TRUE)
Scent = sorted_cent$x
idxScent = sorted_cent$ix
barplot(t(Scent),col="dark red",xlab="NodeNumber", names.arg=idxScent,cex.names=0.75)
print ( diameter(g))
