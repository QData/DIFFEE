library(diffee)

graphics.off()
par(ask=F)
par(mfrow=c(1,1))

readline(prompt="Press [enter] to continue to NIPS word count demo with 2 tasks (before 2006 and after 2006) and 37 features (37 words)")

par(mfrow=c(1,1))

### load nips word count data
data(nip_37_data)

label = colnames(nip_37_data[[1]])

result = diffee(nip_37_data[[1]], nip_37_data[[2]] , 0.0035)

graph = returngraph(result)

### create a fixed layout on cancergraph for plotting
layout = layout_nicely(graph,dim=2)

readline(prompt="Press [enter] to view the difference graph")

plot.diffee(result, graphlabel = label, graphlayout = layout)

nodeid = which(label %in% c("results","section")) ### look for id

readline(prompt="Press [enter] to view the difference graph zooming into node results and section")

plot.diffee(result, graphlabel = label, type = "neighbour",index = nodeid, graphlayout = layout)
