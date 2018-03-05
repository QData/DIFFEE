\name{net.edges}
\alias{net.edges}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
List the edges in the input graph
}
\description{
List every estimated edge in the form of pair of connected nodes  in the input differential network.
}
\usage{
net.edges(theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
An input differential network from two data blocks. The graph is represented as a pXp matrix. (For example, the result of the DIFFEE algorithm: a pXp matrix in which each matrix represents an estimated sparse changes between two Gaussian Graphical Models.)
}
}

\value{
edges, a list, each element of the list represents an igraph.es object which is the detail of all pairs of connected nodes in the input graph.
}
\references{
Beilun Wang, Arshdeep Sekhon, Yanjun Qi (2018). Fast and Scalable Learning of Sparse Changes in High-Dimensional Gaussian Graphical Model Structure. <arXiv:1710.11223>
}
\author{
Beilun Wang}

\examples{
## load an example two datasets, p=100 features, and n=200 samples per dataset:
data(exampleData)
##run diffee
result = diffee(C = exampleData[[1]], D =  exampleData[[2]], 0.05)
## get edges list:
net.edges(result$diffNet)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
