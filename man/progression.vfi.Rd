% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/progression_sources.r
\name{progression.vfi}
\alias{progression.vfi}
\title{progression according to VFI (significant slope, p<=0.05)}
\usage{
progression.vfi(measmatrix)
}
\arguments{
\item{measmatrix}{is a data frame. MUST contain the following columns: 'vfi' (visual field index), 'yearsfollowed', and 'eyeid'. Rows represent the single measurements. The minimum measurements (rows) is 3.}
}
\value{
"stable", "worsening", or "improving" of measurements in \code{timepoints}
}
\description{
\code{progression.vfi} returns the progression criterion used in Aptel et al. (2015).
}
\examples{
data(vf.vfi)
colnames(vf.vfi)
progression.vfi(vf.vfi)
progression.vfi(vf.vfi[vf.vfi$eyeid == 1,])
progression.vfi(vf.vfi[vf.vfi$eyeid == 2,])
}
\seealso{
\url{https://www.ncbi.nlm.nih.gov/pubmed/26095771/}
}
