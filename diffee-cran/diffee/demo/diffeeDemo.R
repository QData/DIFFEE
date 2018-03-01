
pause <- function() {}

### load the diffee library
library(diffee)

pause <- function() {}

### load the cancer data
data(cancer)
X = list(as.matrix(cancer[[1]][which(cancer[[2]] == "not"),]), as.matrix(cancer[[1]][which(cancer[[2]] == "pcr"),]))

### run the diffee
results = diffee(X[[1]], X[[2]], 0.1)
results

pause()

### plot the estimated graphs by diffee
plot.diffee(results)

pause()

### plot the estimated subgraphs that is about a specific node
plot.diffee(results, type="neighbor", index=15)

### load the  example data (larger p)

data(exampleData)
exampleData

pause()

### run simule algorithm to estimate two sparse precision matrices from the exampleData

results = diffee(exampleData[[1]], exampleData[[2]], 0.1)
results


### Output the top-10 hubs in the different network.

print("Output the top-10 hubs in the different network.")
hub = net.hubs(results$diffNet)
hub

pause()

### Output the degrees of nodes in the different network.

print("Output the degrees of nodes in the different network.")
degree = net.degree(results$diffNet)
degree

pause()

### Output the list of edges in the different network.

print("Output the list of edges in the different network.")
edges = net.edges(results$diffNet)
edges

pause()


### Output the neighbors of 50th node in each identified graph.

print("Output the neighbors of 50th node in the different network.")
neighbors = net.neighbors(results$diffNet,index=50)
neighbors

pause()

### plot the estimated graphs by diffee
plot.diffee(results)

pause()

### plot the estimated subgraphs that is about a specific node
plot.diffee(results, type="neighbor", index=15)

