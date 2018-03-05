\name{net.degree}
\alias{net.degree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lists the degree of every node of the differential graph.
}
\description{
Lists the degree of every node of the differential graph.
}
\usage{
net.degree(theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
An input differential network from two data blocks. The graph is represented as a pXp matrix. (For example, the result of the DIFFEE algorithm: a pXp matrix in which each matrix represents an estimated sparse changes between two Gaussian Graphical Models.)
}
}

\value{
Degrees, in the format of a length p vector represents the degree of all p nodes of the graph in the input graph.}
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
## get degree list:
net.degree(result$diffNet)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
