
\name{c3net}
\alias{c3net}

\title{All in one function to infer network with C3NET}
\usage{c3net(dataset, alpha=0.01, methodstep1="cutoff", cutoffMI= 0,
	      fdrmethod="BH", itnum=5, network=FALSE)}
\arguments{
  \item{dataset}{Data set where rows are variables (e.g. genes) and columns are samples.}

  \item{alpha}{Statistical significance threshold}

  \item{itnum}{Number of iterations to resample data to get sampling distribution.}

  \item{methodstep1}{If it is "cutoff" than the input cutoffMI is used as threshold for elimination in step 1. If it is "fdr" then fdrmethod input is used as the specific FDR method to employ. If it is "justp" then only significance threshold alpha is used to compute significance threshold MI.}

  \item{fdrmethod}{It can be any of the options available in the R function p.adjust  (e.g. "holm", "hochberg", "hommel", "bonferroni", "BH", "BY")}

  \item{cutoffMI}{If methodstep1 is set to "cutoff" then this value is used for MI threshold. If this is set to 0 (or by default) it uses mean MI for threshold.}

  \item{network}{If TRUE, the network is plotted in the end.}

}
\value{
  \code{c3net} returns a symmetric mutual information matrix, which is obtained after implementing C3NET. Specifically, non-zero elements in the returned matrix represents undirected link between variables. The inferred network may also be plotted if the argument network is set TRUE. 
}
\description{
  \code{c3net} takes a data set as input and computes the inferred network using C3NET. - see details.
}

\details{
      For Step 1 of C3NET, there are three options that can be used. If methodstep1="cutoff" then cutoffMI input is taken as the significance threshold for step 1. In case cutoffMI is set to 0 then by default mean MI is taken as cutoffMI. If methodstep1 is set to "fdr" then the FDR method from input fdrmethod, itnum for iteration number to get null distribution and alpha for statistical significance is taken for computations. If methodstep1 is set to "justp" or anything else then just alpha and itnum is used for computations. 
 }

\references{
      G. Altay, F. Emmert-Streib, "Inferring the conservative causal core of gene regulatory networks", BMC Systems Biology, 4:132, 2010.    
}

\seealso{\code{\link{makemim}}, \code{\link{copula}}, \code{\link{c3}}, \code{\link{sigtestp}} }

\examples{
data(expdata)
data(truenet)

net <- c3net(expdata, network=TRUE)
scores <- checknet(net,truenet)
}
\keyword{misc}

