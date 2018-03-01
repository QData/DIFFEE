net.edges <- function(theta) {
    adj = .make.adj.matrix(theta,separate=TRUE)
    diag(adj)=0
    gadj = graph.adjacency(adj,mode="upper")
    edges = E(gadj)
    return(edges)
}

