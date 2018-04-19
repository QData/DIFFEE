.checkInv <- function(m) class(try(solve(m),silent=T))=="matrix"

.softThre <- function(x, lambda){
    result = sign(x) * pmax(abs(x)-lambda, 0)
    result
}

.hardThre <- function(x, lambda){
    x[(x != diag(diag(x))) & (abs(x) < lambda)] <- 0
    return(x)
}

.backwardMap <-function(covMatrix, thre = "soft"){
    niuList = 0.001 * (0:1000) * max(covMatrix)
    bestDet = det(.softThre(covMatrix, 0.001))
    bestniu = 0.001

    if (thre == "soft"){
      for (i in 1:1000){
        if (bestDet < det(.softThre(covMatrix, niuList[i]))){
          bestDet = det(.softThre(covMatrix, niuList[i]))
          bestniu = niuList[i]
        }
      }
      return(solve(.softThre(covMatrix, bestniu)))
    }

    if (thre == "hard"){
      for (i in 1:1000){
        if (.checkInv(.hardThre(covMatrix, niuList[i]))){
          bestniu = niuList[i]
          break
        }
      }
      return(solve(.hardThre(covMatrix, bestniu)))
    }
}

diffee <- function(C, D, lambda = 0.05, covType = "cov", thre = "soft"){

    if (is.data.frame(C)){
      C = as.matrix(C)
    }

    if (is.data.frame(D)){
      D = as.matrix(D)
    }
    if (covType == "cov") {
      if (isSymmetric(C) == FALSE){
        covX = cov(C)
      }
      else{
        covX = C
      }

      if (isSymmetric(D) == FALSE){
        covY = cov(D)
      }
      else{
        covY = D
      }
    }

    if (covType == "cor") {
      if (isSymmetric(C) == FALSE){
        covX = cor.fk(C)
      }
      else{
        covX = C
      }

      if (isSymmetric(D) == FALSE){
        covY = cor.fk(D)
      }
      else{
        covY = D
      }
    }


    backX = .backwardMap(covX, thre)
    backY = .backwardMap(covY, thre)
    diffNet = .softThre((backY - backX), lambda)
    out = list(diffNet = diffNet)
    class(out) = "diffee"
    return(out)
}

plot.diffee <-
  function(x, type="graph", index=NULL, ...)
  {
    .env = "environment: namespace:diffee"
    #UseMethod("plot")
    tmp = x$diffNet
    p = dim(tmp)[1]
    if (type == "graph"){
      Graphs = tmp
    }
    if (type == "neighbor"){
      id = matrix(0,p,p)
      id[index,] = rep(1,p)
      id[,index] = rep(1,p)
      for (i in 1:length(tmp)){
        Graphs = tmp * id
      }
    }
    adj = .make.adj.matrix(Graphs)
    diag(adj)=0
    gadj = graph.adjacency(adj,mode="upper",weighted=TRUE)
    #weight the edges according to the classes they belong to
    E(gadj)$color = 2 - get.edge.attribute(gadj,"weight")
    #plot the net using igraph
    plot(gadj, vertex.frame.color="white",layout=layout.fruchterman.reingold,
         vertex.label=NA, vertex.label.cex=3, vertex.size=1)
  }

.make.adj.matrix <- function(theta, separate=FALSE) {
    adj = list()
    if(separate)
    {
      adj = (abs(theta) > 1e-5) * 1
    }
    if(!separate)
    {
      adj = 0*theta
      adj = adj+(abs(theta) > 1e-5) * 1
    }
    return(adj)
  }

