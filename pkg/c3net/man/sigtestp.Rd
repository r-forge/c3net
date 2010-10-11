
\name{sigtestp}
\alias{sigtestp}

\title{Significance test for elimination of nonsignificant edges}
\usage{sigtestp( data, alpha, itnum)}
\arguments{
  \item{data}{Data set where rows are variables (e.g. genes) and columns are samples.}

\item{alpha}{Statistical significance threshold}

\item{itnum}{Number of iterations to resample data to get sampling distribution.}

}
\value{
  \code{sigtestp} returns environment res that contains the obtained threshold value res$I0. 
}
\description{
  \code{sigtestp} takes the data set as input and computes the mutual information matrix in which the nonsignificant elements are eliminated by using only significance threshold alpha. - see details.
}

\details{
      The data set is resampled completely  at each iteration and at the end the sampling distribution is obtained for using in significance test.   
 }

\references{
      G. Altay, F. Emmert-Streib, "Inferring the conservative causal core of gene regulatory networks", BMC Systems Biology,  4:132, 2010.    
}

\seealso{\code{\link{makemim}}, \code{\link{copula}}, \code{\link{c3}}, \code{\link{sigtestfdr}} }

\examples{
data(expdata)

data(truenet)

alpha <- 0.001

itnum <-2

res <- sigtestp( expdata, alpha, itnum)

net <- c3(res$Inew)  # regulatory network inferred (non zero elements stand for links of
		# the predicted network)

scores <- checknet(net,truenet)
}
\keyword{misc}

