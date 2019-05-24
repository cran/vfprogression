#' Combined Visual Field Series for General Progression Method
#'
#' Data
#'
#' @docType data
#'
#' @usage data(vf.schell2014)
#'
#' @format A data frame sample for progression method by Schell et al. 2014, which includes visual field related measurement for two eyes, each with 10 follow-ups. Rows represent the single measurements.
#'
#' @keywords datasets
#'
#' @source
#' \describe{
#'   \item{eyeid}{eyeid, labeled as 1,2... for different eyes.}
#'   \item{md}{mean deviation measurements. The minimum measurements, or rows, for one eye is 4.}
#'   ...
#' }
#'
#' @examples
#' data(vf.schell2014)
#' colnames(vf.schell2014)
#' progression.schell2014(vf.schell2014)
#' progression.schell2014(vf.schell2014[vf.schell2014$eyeid == 1,])
#' progression.schell2014(vf.schell2014[vf.schell2014$eyeid == 2,])
"vf.schell2014"

