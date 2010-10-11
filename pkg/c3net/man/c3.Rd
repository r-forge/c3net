
\name{c3}
\alias{c3}

\title{C3NET second step: Selecting maximum valued elements}
\usage{c3( mim )}
\arguments{
  \item{mim}{A symmetric square mutual information matrix, where i,j th element is the mutual information \eqn{I(i,j)} or \eqn{I(j,i)} between variables i and j.}
}
\value{
  \code{c3} returns a symmetric mutual information matrix, which is obtained after implementing the second step of C3NET algorithm. Specifically, the non-zero elements in the returned matrix represents undirected links between variables if it is statistically significant (the first step of C3NET). 
}
\description{
  \code{c3} takes the mutual information matrix as input and implements the second step of C3NET - see details.
}
\details{
      The C3NET algorithm consists of two main steps. The first step is the same as for relevance networks (RELNET), where all the non-significant mutual information values in the matrix is eliminated statistically if not significant. The second step of C3NET keeps all the maximum valued mutual information values for each row in the matrix and sets the rest of the elements in the matrix zero (the diagonal of the matrix is ignored).   
 }
\references{
      G. Altay, F. Emmert-Streib, "Inferring the conservative causal core of gene regulatory networks", BMC Systems Biology, 4:132, 2010.    
}

\seealso{\code{\link{makemim}}, \code{\link{copula}}, \code{\link{sigtestp}}, \code{\link{sigtestfdr}} }

\examples{
data(expdata)

expdata <- copula(expdata)

mim <- makemim(expdata)

Ic <- mean(mim[upper.tri(mim)])  #Example cut-off for the first step of C3NET

mim[mim < Ic] <-0  #nonsignificant values eliminated wrt C3NET step 1.

net <- c3(mim)  # regulatory network inferred (non zero elements stand for links of
		 #the predicted network)
}
\keyword{misc}

