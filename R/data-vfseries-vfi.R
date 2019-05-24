#' Combined Visual Field Series for General Progression Method
#'
#' Data
#'
#' @docType data
#'
#' @usage data(vf.vfi)
#'
#' @format A data frame for CIGTS progression example, which includes visual field related measurement for two eyes each with 10 follow-ups.
#'
#' @keywords datasets
#'
#' @source
#' \describe{
#'   \item{eyeid}{eyeid, labeled as 1,2... for different eye groups.}
#'   \item{yearsfollowed}{follow-up years. The minimum measurements, or rows, for one eye is 3.}
#'   \item{vfi}{visual field index. The minimum measurements, or rows, for one eye is 3.}
#'   ...
#' }
#'
#' @examples
#' data(vf.vfi)
#' colnames(vf.vfi)
#' progression.vfi(vf.vfi)
#' progression.vfi(vf.vfi[vf.vfi$eyeid == 1,])
#' progression.vfi(vf.vfi[vf.vfi$eyeid == 2,])
"vf.vfi"

