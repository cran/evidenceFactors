\name{mtm}
\alias{mtm}
\docType{data}
\title{
DNA damage from exposure to chromium}
\description{
The data are from a study by Meibian et al. (2008) concerning possible damage to human DNA from occupational exposure to chromium.  There were three matched groups, the control group (cmtm), a low exposure group (e2mtm) and a high exposure group (e1mtm).  The exposed individuals all worked at a tannery where chromium was used in the tanning of leather.  The highly exposed group (e1) worked at tanning leather.  The low exposure group (e2) worked at the same tannery but did not tan leather.  The reported values are the mean tail moment (mtm) of the comet assay, a measure of damage to DNA.  High values of mtm indicate greater damage.
}
\usage{data(mtm)}
\format{
  Each row of mtm is a matched set.  The columns refer to the treatment groups mentioned in the description.
}
\details{
These data were used as an example of approximate evidence factors in Rosenbaum (2011).  Under the null hypothesis H0 of no treatment effect, there are two approximately independent tests of H0 subject to different biases of nonrandom selection, specifically the comparison of 30 controls and 60 matched tannery workers, and the comparison of 30 low and 30 high exposure tannery workers.  The two comparisons may be subjected to sensitivity analyses, say using senmv. The output of senmv for various values of gamma for the two comparisons are used as input in the retentionBrd function. See the documentation for retentionBrd for the example.
}
\source{
Meibian et al. (2008).  Used as an example in Rosenbaum (2011).
}
\references{
Meibian, Z., Zhijian, C., Qing, C. et al. (2008) Investigating DNA damage in tannery workers occupationally exposed to tivalent chromium using the comet assay.  Mutation Research 654, 45-51.

Rosenbaum, P. R. (2011) Some approximate evidence factors in observational studies.  Journal of the American Statistical Association, 2011, 106, 285-295.
}
\examples{
data(mtm)
}
\keyword{datasets}
