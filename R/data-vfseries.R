#' Combined Visual Field Series for General Progression Method
#'
#' Data
#'
#' @docType data
#'
#' @usage data(vfseries)
#'
#' @format A data frame sample including the following visual field related measurement for two eyes, each with 10 follow-ups.
#'
#' @keywords datasets
#'
#' @source
#' \describe{
#'   \item{eyeid}{eyeid, labeled as 1,2... for different eyes.}
#'   \item{nvisit}{number of visits.}
#'   \item{yearsfollowed}{follow-up years.}
#'   \item{distprev}{to be updated.}
#'   \item{age}{in years.}
#'   \item{righteye}{1 as right eye, 0 as left eye.}
#'   \item{malfixrate}{VF test malfixation rate.}
#'   \item{ght}{glaucoma hemifield test result.}
#'   \item{vfi}{visual field index.}
#'   \item{md}{mean deviation.}
#'   \item{mdprob}{mean deviation probability.}
#'   \item{psd}{pattern standard deviation.}
#'   \item{psdprob}{pattern standard deviation probability.}
#'   \item{s1-s54}{52 sensitivity measurements.}
#'   \item{td1-td54 }{52 total deviation measurements.}
#'   \item{tdp1-tdp54}{52 total deviation probability measurements.}
#'   \item{pdp1-pdp54}{52 pattern deviation probability measurements.}
#'   ...
#' }
#' @examples
#' data(vfseries)
#' progression(vfseries)
#' progression(vfseries[vfseries$eyeid == 1,])
#' progression(vfseries[vfseries$eyeid == 2,])
#' progression(vfseries, method=c("cigts"))
#' progression.cigts(vfseries)
#' progression(vfseries, method=c('plr.nouri.2012', 'schell2014', 'vfi'))
"vfseries"

