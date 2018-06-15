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


### function to output diffee result
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

### function to return graph from diffeeresult
returngraph.diffee <-
  function(diffeeresult,
           type = "task",
           index = NULL) {
    .env = "environment: namespace:diffee"
    adj = .make.adj.matrix(diffeeresult$diffNet)
    diag(adj) = 0
    gadj = graph.adjacency(adj, mode = "upper", weighted = TRUE)
    if (!is.null(E(gadj)$weight)) {
      E(gadj)$color = E(gadj)$weight
    }

    if (type == "task"){
      ### do nothing since gadj is already the graph
    }
    else if(type == "neighbour" ){
      if (!prod(index %in% (1:vcount(gadj)))) {
        stop("please specify valid index number(s)")
      }
      gadj = subgraph.edges(gadj,unlist(incident_edges(gadj, index)) , delete.vertices = FALSE)
    }
    else {
      stop("please specify a correct type")
    }
    return(gadj)
  }

if (FALSE){
return.diffee <- function(x,  option = "adjmatrix", type = "graph", index = NULL)
{
  .env = "environment: namespace:simule"
  #UseMethod("plot")
  tmp = x$diffNet
  Graphs = list()
  p = dim(tmp)[1]


  if (type == "graph"){
    Graphs = tmp
  }
  if (type == "neighbor"){
    if (!is.null(index)){
      id = matrix(0,p,p)
      id[index,] = rep(1,p)
      id[,index] = rep(1,p)
      for (i in 1:length(tmp)){
        Graphs = tmp * id
      }
    }
    else{
      print("please specify index (node id)")
      return(NULL)
    }
  }

  adj = .make.adj.matrix(Graphs)
  diag(adj)=0

  if (option == "adjmatrix"){
    return(adj)
  }
  if (option == "igraph"){

    gadj = graph_from_adjacency_matrix(adj,mode="upper",weighted=TRUE)
    E(gadj)$color = get.edge.attribute(gadj,"weight")
    return(gadj)
  }
}
}

### function to plot
plot.diffee <-
  function(diffeeresult, option = "2D", graphlabel = NULL, type="task", index=NULL, graphlayout = NULL, ...)
  {
    .env = "environment: namespace:diffee"

    gadj = returngraph.diffee(diffeeresult, type = type, index = index)

    graphlayout = .makelayout(gadj, option = option, graphlayout = graphlayout)

    title = .maketitle(type = type, index = index, graphlabel = graphlabel)

    if (option == "2D"){
      plot(gadj, layout = graphlayout,
           vertex.label.font=2,
           vertex.shape="none",
           vertex.label.color="gray40",
           vertex.label = graphlabel,
           vertex.label.cex=.7, vertex.frame.color="white", vertex.size = 10 ,main = title )

    }
    else if (option == "3D"){
      rglplot(gadj,layout = graphlayout,
              vertex.label.font=2,
              vertex.shape="none",
              vertex.label.color="gray40",
              vertex.label = graphlabel,
              vertex.label.cex=.7, vertex.size = 10, vertex.frame.color="white", main = title)
    }
    else if (option == "interactive"){
      tkplot(gadj,layout = graphlayout,
             vertex.label.font=2,
             vertex.shape="none",
             vertex.label.color="gray40",
             vertex.label = graphlabel,
             vertex.label.cex=.7, vertex.size = 10, vertex.frame.color="white", main = title)
    }
    else {
      stop("please specify a valid option")
    }

  }


## make adjacency matrix
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

.makelayout <-
  function(x, option = "2D", graphlayout = NULL)
  {
    if (is.null(graphlayout)) {
      if (option == "2D" | option == "interactive"){
        graphlayout = layout_nicely(x,dim=2)

      }
      if (option == "3D"){
        graphlayout = layout_nicely(x,dim=3)
      }
    }
    return(graphlayout)
  }

.maketitle <-
  function(type = "task",
           index = NULL,
           graphlabel = NULL){
    if (type == "task"){
      return ("difference graph")
    }
    if (type == "neighbour"){
       second = "on difference graph"
      if (is.null(graphlabel) || is.na(graphlabel)) {
        first = paste("Zoom in at node", paste(as.character(index), collapse = ", "))
      }

      else {
        first = paste("Zoom in at node", paste(as.character(graphlabel[index]), collapse = ", "))
      }
    }
    return (paste(first,second))

  }
