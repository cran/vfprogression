#' Combined Visual Field Series for General Progression Method
#'
#' Data
#'
#' @docType data
#'
#' @usage data(vf.plr.nouri.2012)
#'
#' @format A data frame sample for Pointwise Linear Regression (PLR) method according to Nouri-Mahdavi 2012 progression, which includes visual field related measurement for two eyes, each with 10 follow-ups. Rows represent the single measurements.
#'
#' @keywords datasets
#'
#' @source
#' \describe{
#'   \item{eyeid}{eyeid, labeled as 1,2... for different eyes}
#'   \item{yearsfollowed}{follow-up years. The minimum measurements, or rows, for one eye is 3}
#'   \item{td1-td54}{52 total deviation, or 'td' measurements. The minimum measurements, or rows, for one eye is 3}
#'   ...
#' }
#'
#' @examples
#' data(vf.plr.nouri.2012)
#' colnames(vf.plr.nouri.2012)
#' progression.plr.nouri.2012(vf.plr.nouri.2012)
#' progression.plr.nouri.2012(vf.plr.nouri.2012[vf.plr.nouri.2012$eyeid == 1,])
#' progression.plr.nouri.2012(vf.plr.nouri.2012[vf.plr.nouri.2012$eyeid == 2,])
"vf.plr.nouri.2012"

