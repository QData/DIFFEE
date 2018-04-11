\name{diffee-package}
\alias{diffee-package}
\docType{package}
\title{
estimating DIFFerential networks via an Elementary
Estimator under a high-dimensional situation}
\description{
This is an R implementation of Fast and Scalable Learning of Sparse Changes in High-Dimensional Gaussian Graphical Model Structure (DIFFEE). The DIFFEE algorithm can be used to fast estimate the differetial network between two related datasets. For instance, it can identify differential gene network from datasets of case and control. By performing data-driven network inference from two high-dimensional data sets, this tool can help users effectively translate two aggregated data blocks into knowledge of the changes among entities between two Gaussian Graphical Model. Please run demo(diffeeDemo) to learn the basic functions provided by this package. For further details, please read the original paper: Beilun Wang, Arshdeep Sekhon, Yanjun Qi (2018) <arXiv:1710.11223>.
}
\details{
\tabular{ll}{
Package: \tab diffee\cr
Type: \tab Package\cr
Version: \tab 1.0.0\cr
Date: \tab 2018-03-05\cr
License: \tab GPL (>= 2)\cr
}
We focus on the problem of estimating the change in the dependency structures of two p-dimensional Gaussian Graphical models (GGMs). Previous studies for sparse change estimation in GGMs involve expensive and difficult non-smooth optimization. We propose a novel method, DIFFEE for estimating DIFFerential networks via an Elementary Estimator under a high-dimensional situation. DIFFEE is solved through a faster and closed form solution that enables it to work in large-scale settings. We conduct a rigorous statistical analysis  showing that surprisingly DIFFEE achieves the same asymptotic convergence rates as the state-of-the-art estimators that are much more difficult to compute. Our experimental results on multiple synthetic datasets and one real-world data about brain connectivity show strong performance improvements over baselines, as well as significant computational benefits.
}
\author{
Beilun Wang

Maintainer: Beilun Wang - bw4mw at virginia dot edu
}
\references{
Beilun Wang, Arshdeep Sekhon, Yanjun Qi (2018). Fast and Scalable Learning of Sparse Changes in High-Dimensional Gaussian Graphical Model Structure. <arXiv:1710.11223>
}
\keyword{ package }
\examples{
data(exampleData)
diffee(C = exampleData[[1]], D =  exampleData[[2]], 0.05)
}
