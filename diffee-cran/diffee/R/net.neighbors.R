net.neighbors <- function(theta,index) {
    #index = (row.names(theta[[1]])==name)
    p = dim(theta)[1]
    neighbors = which(theta[index,]!=0)
    return(neighbors)
  }

