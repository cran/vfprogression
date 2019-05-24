#' Combined Visual Field Series for General Progression Method
#'
#' Data
#'
#' @docType data
#'
#' @usage data(vf.cigts)
#'
#' @format A data frame sample for CIGTS progression method, which includes visual field related measurement for two eyes, each with 10 follow-ups. Rows represent the single measurements.
#'
#' @keywords datasets
#'
#' @source
#' \describe{
#'   \item{eyeid}{eyeid, labeled as 1,2... for different eyes.}
#'   \item{yearsfollowed}{follow-up years. The minimum measurements /rows for one eye is 5.}
#'   \item{tdp1-tdp54}{52 total deviation probability, or 'tdp' measurements. The minimum measurements, or rows for one eye is 5.}
#'   ...
#' }
#'
#' @examples
#' data(vf.cigts)
#' colnames(vf.cigts)
#' progression.cigts(vf.cigts)
#' progression.cigts(vf.cigts[vf.cigts$eyeid == 1,])
#' progression.cigts(vf.cigts[vf.cigts$eyeid == 2,])
"vf.cigts"

