\name{plotRejDecbyAssm}
\alias{plotRejDecbyAssm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots labelling which assumption provides the evidence
}
\description{
For two evidence factors this function plots the assumptions for which the 
hypothesis is rejected in a sensitivity analysis.
}
\usage{
plotRejDecbyAssm(retentionBrd, Gamlist, Plist, alpha = 0.05, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{retentionBrd}{
	Output of retentionBrd function for two evidence factors.
	}
  \item{Gamlist}{
	List of two numeric vectors of gamma values for two evidence factors.
	}
  \item{Plist}{
	A list of two numeric vectors. Each vector are the maximum p-values for evidence factors for sensitivity analysis for increasing gamma values.
	}
  \item{alpha}{
	Size of overall type-1 error in analysis. Should be same as in retentionBrd function.
	}
	\item{...}{
	Further arguments to be passed to method image and legend used for plotting.  
	}
}
\details{
The aim of this function is to assign the rejection decision to one or either of the assumptions for each couple of bias levels.

plotRejDecbyAssm uses a closed testing ideology to produce another grey-scale plot where further shading is used to identify the rejection decisions to one or both of the assumptions. See Karmakar et. al. (2016) for detailed discussion.
}
\value{
A grey-scale plot similar to the output of plotRetentionArea. 
}
\references{
Karmakar, B., French, B., Sadakane, A. and Small, D. S. (2016) Integrating the evidence from evidence factors in observational studies. 
}
\author{
\packageAuthor{evidenceFactors}
}

\seealso{
\code{\link{plotRetentionArea}}
}
\examples{

data(Plist)
data(Gamlist)
rbrd = retentionBrd(Plist, Gamlist)
plotRejDecbyAssm(rbrd, Gamlist, Plist)

rbrd = retentionBrd(Plist, Gamlist, method = "Truncated")
plotRejDecbyAssm(rbrd, Gamlist, Plist)

## mean tail moment data example
library(sensitivitymv)

data(mtm)
Gamseq <- seq(1, 15, by = 0.2)
Gamlist <- list(Gamseq, Gamseq)
Plist <- list(c(), c())

for(gam in Gamseq){
	Plist[[1]] = c(Plist[[1]], senmv(-mtm,gamma=gam,trim=1)$pval)
	Plist[[2]] = c(Plist[[2]], senmv(-mtm[,2:3],gamma=gam,trim=1)$pval)
}	

# Fisher's combination method
rbrd <- retentionBrd(Plist, Gamlist)
plotRejDecbyAssm(rbrd, Gamlist, Plist)

# truncated product combination
rbrd <- retentionBrd(Plist, Gamlist, method = "Truncated", talpha = .5)
plotRejDecbyAssm(rbrd, Gamlist, Plist)
}

\keyword{ Sensitivity Analysis }
\keyword{ Evidence Factors }
