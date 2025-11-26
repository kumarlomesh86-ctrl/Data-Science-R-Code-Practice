library(igraph)
g <- sample_gnp(n = 20, p = 1/20, directed = FALSE)
g
clusters(g)
plot.igraph(g)
