net.degree <- function(theta) {
    degree = 0
      degree = (rowSums(abs(theta)>1e-5)-1)
    return(degree)
}
