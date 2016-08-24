\name{retentionBrd}
\alias{retentionBrd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes the Multiplicity Controlled Retention Set.
}
\description{
Computes the lower boundary of convex retention set of sensitivity analysis for evidence factors.
}
\usage{
retentionBrd(Plist, Gamlist, alpha = 0.05, method = c("Fisher", "TruncatedP"),
 talpha = .2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Plist}{
	A list of numeric vectors. Each vector are maximum p-values for evidence factors for sensitivity analysis for increasing gamma values. 
}
  \item{Gamlist}{
	A list of numeric vectors. gamma values associated with evidence factors.
}
  \item{alpha}{
	Size of overall type-1 error in analysis.
}
	\item{method}{
	Combination method of evidence factors. Default choice is Fisher's combination.
	Another option is "TruncatedP" for the truncated product method.
}
	\item{talpha}{
	Level of truncation for truncated product ("TruncatedP") combination method.
	Default is 0.20.
}
}
\details{
Vectors in Plist and Gamlist are necessarily increasing. 

The reporting of the sensitivity analysis also controlled for overall type-1 error. The retention set in a sensitivity analysis of evidence factors is a convex increasing region. The output of the algorithm is the lower boundary of the retention region. The complexity of the algorithm is linear in number of evidence factors and log-linear in the range of gamma. The pseudo-code for this function with discussion of the complexity of the algorithm is available in Karmakar et. al. (2016).

Two methods of combing methods can be used, "Fisher" and "TruncatedP". For details on the two methods see Karmakar et. al. (2016). The truncated method of combing p-values was first introduced by Zaykin et al. (2002). In practice the truncated product method is recommended even though neither one uniformly dominates the other in terms of power.  

Packages sensitivitymv, sensitivitymw and sensitivity2x2xk are various CRAN packages that can be used to compute Plist from dat and Gamlist. See example codes below.
}
\value{
A matrix with number of columns same as the number of evidence factors. This matrix identifies the border of the retention region.
}

\references{
Karmakar, B., French, B., Sadakane, A. and Small, D. S. (2016) Integrating the evidence from evidence factors in observational studies. 

Rosenbaum, P. R. (2002) Observational Studies (2nd edition).  New York: Springer.

Rosenbaum, P. R. (2010)  Evidence factors in observational studies. Biometrika, 97, 333-345.

Rosenbaum, P. R. (2011) Some approximate evidence factors in observational studies.  Journal of the American Statistical Association, 106, 285-295.

Zaykin, D., Zhivotovsky, L. A., Westfall, P. and Weir, B. (2002) Truncated product
method for combining p-values. Genetic Epidemiology, 22, 170-1
}
\author{
\packageAuthor{evidenceFactors}
}
\examples{

data(Plist)
data(Gamlist)
retentionBrd(Plist, Gamlist)
retentionBrd(Plist, Gamlist, method = "Truncated")

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

# truncated product combination
rbrd <- retentionBrd(Plist, Gamlist, method = "Truncated", talpha = .5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Sensitivity Analysis }
\keyword{ Evidence Factors }
