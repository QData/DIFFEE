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
      E(gadj)$color = rainbow(1)[E(gadj)$weight]
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


### function to plot
plot.diffee <-
  function(diffeeresult,
           graphlabel = NULL,
           type = "task",
           index = NULL,
           graphlayout = NULL,
           ...)
  {
    .env = "environment: namespace:diffee"

    gadj = returngraph.diffee(diffeeresult, type = type, index = index)

    graphlayout = .makelayout(gadj, graphlayout = graphlayout)

    title = .maketitle(type = type,
                       index = index,
                       graphlabel = graphlabel)

    plot(
      gadj,
      layout = graphlayout,
      vertex.label.font = 2,
      vertex.shape = "none",
      vertex.label.color = "gray40",
      vertex.label = graphlabel,
      vertex.label.cex = .7,
      vertex.frame.color = "white",
      vertex.size = 10 ,
      main = title
    )

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
  function(x, graphlayout = NULL)
  {
    if (is.null(graphlayout)) {
      graphlayout = layout_nicely(x, dim = 2)

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
