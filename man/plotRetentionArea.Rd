\name{plotRetentionArea}
\alias{plotRetentionArea}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots the convex increasing retention area
}
\description{
For two evidence factors this function produces a grey-scale plot of the retention set, i.e. the set of sensitive bias levels.
}
\usage{
plotRetentionArea(retentionBrd, Gamlist, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{retentionBrd}{
	Output of retentionBrd function for two evidence factors.
}
  \item{Gamlist}{
	List of two vectors of gamma values for two evidence factors.
}
\item{...}{
	Further arguments to be passed to method image and legend used for plotting.  
	}
}
\details{
 For 2 evidence factors plotRetentionArea produces a grey-scale plot of the retention set, i.e. the set of sensitive bias levels.
}
\value{
The output of the function is plot.

}
\references{
Karmakar, B., French, B., Sadakane, A. and Small, D. S. (2016) Integrating the evidence from evidence factors in observational studies. 
}
\author{
\packageAuthor{evidenceFactors}
}

\seealso{
\code{\link{plotRejDecbyAssm}}.
}
\examples{

data(Plist)
data(Gamlist)
rbrd = retentionBrd(Plist, Gamlist)
plotRetentionArea(rbrd, Gamlist)

rbrd = retentionBrd(Plist, Gamlist, method = "Truncated")
plotRetentionArea(rbrd, Gamlist)

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
plotRetentionArea(rbrd, Gamlist)

# truncated product combination
rbrd <- retentionBrd(Plist, Gamlist, method = "Truncated", talpha = .5)
plotRetentionArea(rbrd, Gamlist)
}

\keyword{ Sensitivity Analysis }
\keyword{ Evidence Factors }
