
\name{sigtestfdr}
\alias{sigtestfdr}

\title{Significance test for elimination of nonsignificant edges with FDR}
\usage{sigtestfdr( data, alpha, itnum, methodsig="BH")}
\arguments{
  \item{data}{Data set where rows are variables (e.g. genes) and columns are samples.}

\item{alpha}{Significance threshold}

\item{itnum}{Number of iterations to resample data to get sampling distribution.}

\item{methodsig}{A FDR method.}

}
\value{
  \code{sigtestfdr} returns environment res that contains the new MI matrix, res$Inew, that is obtained after elimination of nonsignificant elements with FDR. 
}
\description{
  \code{sigtestfdr} takes the data set as input and computes the mutual information matrix in which the nonsignificant elements are eliminated by a FDR method. - see details.
}

\details{
      The data set  is resampled completely at each iteration and at the end the sampling distribution is obtained for using in FDR.   
 }

\references{
      G. Altay, F. Emmert-Streib, "Inferring the conservative causal core of gene regulatory networks", BMC Systems Biology,  4:132, 2010.    
}

\seealso{\code{\link{makemim}}, \code{\link{copula}}, \code{\link{c3}}, \code{\link{sigtestp}} }

\examples{
data(expdata)

data(truenet)

alpha <- 0.001

itnum <-2

res <- sigtestfdr( expdata, alpha, itnum, methodsig="BH")

net <- c3(res$Inew)  # regulatory network inferred (non zero elements stand for links of
		# the predicted network)

scores <- checknet(net,truenet)
}
\keyword{misc}

