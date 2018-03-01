net.hubs <- function(theta,nhubs=10) {
    degree = net.degree(theta)
    order = order(degree,decreasing=TRUE)
    hubs = degree[order[1:nhubs]]
    return(hubs)
  }
