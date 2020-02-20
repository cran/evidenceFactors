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

\packageIndices{evidenceFactors}
The main function of the evidenceFactors package is retentionBrd.

This package provides tool for reporting of evidence factors analysis in observational studies. Implementation  of the functions in this package depends on the computation of maximum p-values for different bias levels for all the evidence factors. Based on the vector of maximum p-values the functions in the package can be used to efficiently perform sensitivity analysis.  

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
Karmakar, B., Doubeni, C. A. and Small, D. S. (2020) Evidence factors for in
case-control study with application to the effect of flexible 
sigmiodoscopy screening on colorectal cancer, Annals of Applied Statistics, forthcomming.

Karmakar, B., French, B. and Small, D. S. (2019) Integrating the evidence from evidence factors in observational studies, Biometrika 106 353-367. 

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


\dontrun{
# Other usages
## Pseudocode for the analysis of the Sigmoidoscopy study in 
## Karmakar, Doubeni and Small (2020).
library(haven)

data_reduced = read_dta('matched_cc_analysis.dta')

data_reduced$casetype=NA
data_reduced$casetype[data_reduced$casetype_c== -1]='Controls'
 data_reduced$casetype[data_reduced$casetype_c== 0]='Proximal cancer cases'
 data_reduced$casetype[data_reduced$casetype_c== 1]='Distal cancer cases'
data_reduced$casetype = as.factor(data_reduced$casetype)

data_reduced$fsg <- NA
data_reduced$fsg[data_reduced$fsg_bk==0]='No'
data_reduced$fsg[data_reduced$fsg_bk==1]='Yes'
data_reduced$fsg = as.factor(data_reduced$fsg)

data_reduced$caseflg <- NA
data_reduced$caseflg[data_reduced$caseflg_bk==0]='No'
data_reduced$caseflg[data_reduced$caseflg_bk==1]='Yes'



## Forming the contingency array.
## 
stratas <- unique(data_reduced$strata_id)
nstrata = length(stratas)

tab2x2xk_incases <- array(NA, c(2,2,nstrata))
tab2x2xk <- array(NA, c(2,2,nstrata))

dimnames(tab2x2xk) = list(fsg=c('Yes','No'), 
				casetype=c('Controls', 'Cases'), stratas=stratas)
dimnames(tab2x2xk_incases) = list(fsg=c('Yes','No'), 
				casetype=c('Proximal', 'Distal'), stratas=stratas)

for(s in stratas){
	data.strata = data_reduced[data_reduced$strata_id==s, 
								c('fsg', 'casetype')]
	temptab = table(data.strata$fsg, data.strata$casetype)
	
	tab2x2xk_incases[,,s] = temptab[c('Yes', 'No'), 
			c('Proximal cancer cases', 'Distal cancer cases')]

	tab2x2xk[,'Controls',s] = temptab[c('Yes', 'No'), 'Controls']
	tab2x2xk[,'Cases',s] = temptab[c('Yes', 'No'), 'Proximal cancer cases'] + 
						temptab[c('Yes', 'No'), 'Distal cancer cases']
	
}

library(sensitivity2x2xk)
## calculating the randomized p-values
mh(tab2x2xk_incases)
mh(tab2x2xk)

## sensitivity analysis
mh(tab2x2xk_incases, Gamma=1.5)
mh(tab2x2xk, Gamma=1.3)


}



## Simulating a case referent study from an infinite cohort.
## Replication code for the simualtion results of Section 5.1 of
## Karmakar, Doubeni and Small (2020).


# Treatment assignment in the favourable situation with
# P(Z = 1) = 1/3

# treatment effect delta

# Fix the sample size for the broad cases
# and the referents nB and nR respectively.


##################Finding quantiles
#### t3 distribution
# library(AdMit)
# mit <- list(p = c(2/3, 1/3),
#              mu = matrix(c(0, .5), 2, 1, byrow = TRUE),
#              Sigma = matrix(c(1, 1), 2),
#              df = 3)
#X <- rMit(1000, mit)
# quantile(X, .8)

#### Normal distribution
# library(ks)
# X <- rnorm.mixt(1000, mus = c(0, 0.5), sigmas=c(1, 1), props = c(2/3, 1/3))
# quantile(X, .8)
##################

library(sensitivitymv)

\dontshow{
N = 1
}

\dontrun{
N = 10000
}

effSeq = seq(0, 1, by = .2)
gamSeq = seq(1, 4.5, by =.25)
pZ = 1/3


rejFreqFisher <- array(0, c(length(effSeq), length(gamSeq), length(gamSeq)) )
rejFreqTP <- array(0, c(length(effSeq), length(gamSeq), length(gamSeq)) )


nB = 2000
nR = 2000


for(Itr in 1:N){
	for(betaIdx in 1:length(effSeq)){
		beta = effSeq[betaIdx]
		ZB = rep(NA, nB)
		ZR = rep(NA, nR)

		ResB = rep(NA, nB)
		ResR = rep(NA, nR)

		Bdef = 1.17		#1.031
		countB = 0
		countR = 0

		while(countB < nB | countR < nR){
			Ztemp =  sample(c(0,1), 1, prob = c((1-pZ), pZ))

			# Response Distribution 
			 Rtemp = rnorm(1) + ifelse(Ztemp==1, beta, 0)	
			#Rtemp = rt(1, 3)/sqrt(3) + ifelse(Ztemp==1, beta, 0)


			if(Rtemp > Bdef){
				if(countB < nB){
					countB = countB + 1
					ZB[countB] = Ztemp
					ResB[countB] = Rtemp
				}
			}

			if(Rtemp <= Bdef){
				if(countR < nR){
					countR = countR + 1
					ZR[countR] = Ztemp
					ResR[countR] = Rtemp
				}
			}
		}


		## Simulation done

		# Lets first consider the p-value based on the Broad Referent comparison
		exposedBR = ZB + ZR
		BRcase_sizes = 1

		# Now the Narrow vs Merginal comparison
		# First defining narrow cases

		narrow = ResB > median(ResB)
		marginal = ResB < median(ResB)

		exposedNM = ZB[narrow] + ZB[marginal]
		NMcase_sizes = 1
		J = 2

		
		for(gam1 in 1:length(gamSeq)){
			for(gam2 in 1:length(gamSeq)){
				Gam1 = gamSeq[gam1]
				Gam2 = gamSeq[gam2]

				pNM = Gam1*exposedNM/(Gam1*exposedNM + (J - exposedNM))
				evidenceNM = pnorm((sum(ZB[narrow]) - 
					sum(NMcase_sizes*pNM))/sqrt(sum(NMcase_sizes*pNM*(1-pNM))), 
						lower.tail=FALSE)

				pBR = Gam2*exposedBR/(Gam2*exposedBR + (J - exposedBR))
				evidenceBR = pnorm((sum(ZB) - 
					sum(BRcase_sizes*pBR))/sqrt(sum(BRcase_sizes*pBR*(1-pBR))), 
						lower.tail=FALSE)

				Pvec = c(evidenceNM, evidenceBR)

				rejFreqFisher[betaIdx,gam1,gam2]=rejFreqFisher[betaIdx,gam1,gam2]+
						( pchisq(-2*log(prod(Pvec)), 2*length(Pvec), 
								lower.tail=FALSE) < 0.05 )

				rejFreqTP[betaIdx , gam1, gam2] <- rejFreqTP[betaIdx, gam1, gam2] + 
						( truncatedP(Pvec) < 0.05 )
			}
		}

	}
	cat(Itr, " ")
	#if(!(Itr %% 50)) cat("\n")
}

}
