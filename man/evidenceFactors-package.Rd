\name{evidenceFactors-package}
\alias{evidenceFactors-package}
\alias{evidenceFactors}
\docType{package}
\title{
\packageTitle{evidenceFactors}
}
\description{
\packageDescription{evidenceFactors}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{evidenceFactors}
\packageIndices{evidenceFactors}
The main function of the  evidenceFactors package is retentionBrd.

This evidenceFactors package provides tool for reporting of evidence factors analysis in observational studies. Implementation  of the functions in this package depends on the computation of maximum p-values for different bias levels for all the evidence factors. Based on the vector of maximum p-values the functions in the package can be used to efficiently perform sensitivity analysis.  

The sensitivity analysis asks about the magnitude, gamma, of bias in treatment assignment in observational studies that would need to be present to alter the conclusions of a randomization test that assumed matching for observed covariates removes all bias. The tools and algorithms implemented in evidenceFactors package are discussed in detail in Karmakar et. al. (2016).  For general discussion of sensitivity analyses in observational studies, see Chapter 4 of Rosenbaum (2002).

The main function retentionBrd takes as input a list of the maximum p-values for each evidence factors for a range of gamma values. The retention set in such sensitivity analysis is a convex increasing region. The output of the algorithm is the lower boundary of the retention region. The complexity of the algorithm is linear in number of evidence factors and log-linear in the range of gamma. Either fisher's combination method or the truncated product method (Zaykin et al., 2002) can be used as combining method for evidence factors.

The other two functions in the package are plotting functions. For 2 evidence factors plotRetentionArea produces a grey-scale plot of the retention set, i.e. the set of sensitive bias levels. plotRejDecbyAssm uses a closed testing ideology to produce another grey-scale plot where further shading is used to identify the rejection decisions to one or both of the assumptions. See Karmakar et. al. (2016) for detailed discussion.

Packages sensitivitymv, sensitivitymw and sensitivity2x2xk are various CRAN packages that can be used to compute Plist from dat and Gamlist. See example codes below.
}
\author{
\packageAuthor{evidenceFactors}

Maintainer: \packageMaintainer{evidenceFactors}
}

\references{
Karmakar, B., French, B., Sadakane, A. and Small, D. S. (2016) Integrating the evidence from evidence factors in observational studies. 

Rosenbaum, P. R. (2002) Observational Studies (2nd edition).  New York: Springer.

Rosenbaum, P. R. (2010)  Evidence factors in observational studies. Biometrika, 97, 333-345.

Rosenbaum, P. R. (2011) Some approximate evidence factors in observational studies.  Journal of the American Statistical Association, 106, 285-295.

Zaykin, D., Zhivotovsky, L. A., Westfall, P. and Weir, B. (2002) Truncated product
method for combining p-values. Genetic Epidemiology, 22, 170-185.
}

\keyword{ package }

\examples{
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
plotRejDecbyAssm(rbrd, Gamlist, Plist, alpha = 0.05)

# truncated product combination
rbrd <- retentionBrd(Plist, Gamlist, method = "Truncated", talpha = .5)
plotRetentionArea(rbrd, Gamlist)
plotRejDecbyAssm(rbrd, Gamlist, Plist, alpha = 0.05)
}
