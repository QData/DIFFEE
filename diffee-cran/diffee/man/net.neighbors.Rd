\name{net.neighbors}
\alias{net.neighbors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get neighbors of a node in the input graph.
}
\description{
For the graph in the input graph, returns the name of neighbor nodes connected to a given node.
}
\usage{
net.neighbors(theta, index)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
  An input differential network from two data blocks. The graph is represented as a pXp matrix. (For example, the result of the DIFFEE algorithm: a pXp matrix in which each matrix represents an estimated sparse changes between two Gaussian Graphical Models.)
}
  \item{index}{
The row number of the node to be investigated.
}
}

\value{
neighbors, a vector including row names of the neighbor nodes for the index node in the input graph.
}
\references{
Beilun Wang, Arshdeep Sekhon, Yanjun Qi (2018). Fast and Scalable Learning of Sparse Changes in High-Dimensional Gaussian Graphical Model Structure. <arXiv:1710.11223>
}
\author{
Beilun Wang}

\examples{
## load an example two datasets, p=100 features, and n=200 samples per dataset:
data(exampleData)
##run simule
result = diffee(C = exampleData[[1]], D =  exampleData[[2]], 0.05)
## get neighbors of node 50:
net.neighbors(result$diffNet,index=50)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
