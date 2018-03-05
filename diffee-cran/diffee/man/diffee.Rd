\name{diffee}
\alias{diffee}
\title{Fast and Scalable Learning of Sparse Changes in High-Dimensional Gaussian Graphical Model Structure}
\usage{
diffee(C, D, lambda = 0.05)
}
\arguments{
\item{C}{A input matrix for the 'control' group. It can be data matrix or covariance matrix. If C is a symmetric matrix, the matrices are assumed to be covariance matrix. More details at <https://github.com/QData/DIFFEE>}
\item{D}{A input matrix for the 'disease' group. It can be data matrix or covariance matrix. If D is a symmetric matrix, the matrices are assumed to be covariance matrix. More details at <https://github.com/QData/DIFFEE>}
\item{lambda}{A positive number. The hyperparameter controls the sparsity level of the matrices. The \eqn{\lambda_n} in the following section: Details.}
}
\description{
Estimate DIFFerential networks via an Elementary
Estimator under a high-dimensional situation. Please run demo(diffeeDemo) to learn the basic functions provided by this package.  For further details, please read the original paper:  Beilun Wang, Arshdeep Sekhon, Yanjun Qi (2018) <arXiv:1710.11223>.
}
\value{
\item{diffNet}{A matrix of the estimated sparse changes between two Gaussian Graphical Models}
}
\details{
  The DIFFEE algorithm is a fast and scalable Learning algorithm of Sparse Changes in High-Dimensional Gaussian Graphical Model Structure. It solves the following equation:
\deqn{
\min\limits_{\Delta}||\Delta||_1
}
Subject to :
\deqn{
  ([T_v(\hat{\Sigma}_{d})]^{-1} - [T_v(\hat{\Sigma}_{c})]^{-1})||_{\infty} \le \lambda_n
}
Please also see the equation (2.11) in our paper. The \eqn{\lambda_n} is the hyperparameter controlling the sparsity level of the matrix and it is the \code{lambda} in our function. For further details, please see our paper: Beilun Wang, Arshdeep Sekhon, Yanjun Qi (2018) <arXiv:1710.11223>.
}
\author{
Beilun Wang
}
\references{
Beilun Wang, Arshdeep Sekhon, Yanjun Qi (2018). Fast and Scalable Learning of Sparse Changes in High-Dimensional Gaussian Graphical Model Structure. <arXiv:1710.11223>
}
\examples{
data(exampleData)
results = diffee(C = exampleData[[1]], D =  exampleData[[2]], 0.05)
plot.diffee(results)
plot.diffee(results, type="neighbor", index=50)
}
