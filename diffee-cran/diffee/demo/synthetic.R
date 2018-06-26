library(diffee)

graphics.off()
par(ask=F)
par(mfrow=c(1,1))

readline(prompt="Press [enter] to continue to synthetic Gaussian data demo with 2 tasks and 20 features")


### load the  example data
data(exampleData)


### run simule algorithm to estimate two sparse precision matrices from the exampleData
result = diffee(exampleData[[1]], exampleData[[2]], 0.45)


### plot the estimated graphs by diffee

data(exampleDataGraph)
par(mfrow=c(1,2))


layout = layout_nicely(exampleDataGraph[[1]], dim = 2)

readline(prompt="Press [enter] to view comparison between ground truth and generated difference graph")
{
  plot.diffee(result,graphlayout = layout)

  plot.igraph(union(exampleDataGraph[[2]], exampleDataGraph[[3]]),layout = layout,vertex.label.font=2,
              vertex.shape="none",
              vertex.label.color="gray40",
              vertex.label.cex=.7, vertex.frame.color="white", vertex.size = 10 ,
              edge.color = rainbow(1)[1],
              main = "difference graph ground truth")
}

nodeid = c(3,8)

par(mfrow=c(1,1))

readline(prompt="Press [enter] to view difference graph zooming into node 3 and 8")

plot.diffee(result,type = "neighbour", index = nodeid, graphlayout = layout)


readline(prompt="Press [enter] to continue to synthetic Gaussian data demo with 2 tasks and 500 features")


data(exampleData500)

readline(prompt="Press [enter] to view the DIFFEE runtime (takes roughly 30 seconds on an i7 machine)")
{
  start_time = Sys.time()
  diffee(exampleData500[[1]], exampleData500[[2]], 1)
  end_time = Sys.time()
  print(paste("the time taken for DIFFEE to run with p = 500 is", end_time - start_time, "seconds"))
}

