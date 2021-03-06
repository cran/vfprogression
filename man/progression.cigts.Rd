% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/progression_sources.r
\name{progression.cigts}
\alias{progression.cigts}
\title{CIGTS VF progression}
\usage{
progression.cigts(measmatrix)
}
\arguments{
\item{measmatrix}{is a data frame. MUST contain the following columns: 52/54 TD probs (column names MUST be 'tdp1' ~ 'tdp52' or 'tdp1' ~ 'tdp54'), 'yearsfollowed', and 'eyeid'. Rows represent the single measurements. The minimum measurements (rows) is 5.}
}
\value{
"stable", "worsening", or "improving" of measurements in \code{measmatrix}. Note: If a VF series is temporarily improving and temporarily worsening, it is assumed to be "stable" overall
}
\description{
\code{progression.cigts} returns the progression of visual field test based on 52 or 54 total deviation probabilities (tdp). CIGTS VF progression (Musch et al., 1999).
}
\examples{
data(vf.cigts)
colnames(vf.cigts)
progression.cigts(vf.cigts)
progression.cigts(vf.cigts[vf.cigts$eyeid == 1,])
progression.cigts(vf.cigts[vf.cigts$eyeid == 2,])
}
\references{
\url{http://www.aaojournal.org/article/S0161-6420(99)90147-1/abstract}
}
