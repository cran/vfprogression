######## Progression source code
#Dian 6/16/2017

#### AGIS VF scoring

# according to Fig. 1 in AGIS 2 (Gaasterland et al., 1994)
agis.vf.sectors <- c(
	rep("upper", 10),
	"nasal", rep("upper", 7),
	"nasal", "nasal", rep("upper", 5), NA, "upper",
	"nasal", "nasal", rep("lower", 5), NA, "lower",
	"nasal", rep("lower", 7),
	rep("lower", 10))

# neighboring VF locations within each of the three VF sectors,
# according to Fig. 1 in AGIS 2 (Gaasterland et al., 1994)
agis.neighbors <- list(
	c(2, 5, 6, 7), c(1, 3, 6:8), c(2, 4, 7:9), c(3, 8:10),
	c(1, 6, 12, 13), c(1, 2, 5, 7, 12:14), c(1:3, 6, 8, 13:15), c(2:4, 7, 9, 14:16), c(3, 4, 8, 10, 15:17), c(4, 9, 16:18),
	c(19, 20), c(5, 6, 13, 21, 22), c(5:7, 12, 14, 21:23), c(6:8, 13, 15, 22:24), c(7:9, 14, 16, 23:25), c(8:10, 15, 17, 24, 25), c(9, 10, 16, 18, 25, 27), c(10, 17, 27),
	c(11, 20, 28, 29), c(11, 19, 28, 29), c(12, 13, 22), c(12:14, 21, 23), c(13:15, 22, 24), c(14:16, 23, 25), c(15:17, 24), NA, c(17, 18),
	c(19, 20, 29, 37), c(19, 20, 28, 37), c(31, 38, 39), c(30, 32, 38:40), c(31, 33, 39:41), c(32, 34, 40:42), c(33, 41:43), NA, c(43, 44),
	c(28, 29), c(30, 31, 39, 45, 46), c(30:32, 38, 40, 45:47), c(31:33, 39, 41, 46:48), c(32:34, 40, 42, 47:49), c(33, 34, 41, 43, 48:50), c(34, 36, 42, 44, 49, 50), c(36, 43, 50),
	c(38, 39, 46, 51), c(38:40, 45, 47, 51, 52), c(39:41, 46, 48, 51:53), c(40:42, 47, 49, 52:54), c(41:43, 48, 50, 53, 54), c(42:44, 49, 54),
	c(45:47, 52), c(46:48, 51, 53), c(47:49, 52, 54), c(48:50, 53))

agis.is.abnormal <- function(vf)
# returns boolean vector of VF locations;
# TRUE: abnormal according to  AGIS 2 (Gaasterland et al., 1994)
{
	x = if(length(vf)<54) c(vf[1:25], NA, vf[26:33], NA, vf[34:52])  else vf
	# according to Fig. 1 in AGIS 2 (Gaasterland et al., 1994):
	criteria <- -c(
		rep(9, 4),
		rep(8, 8), rep(6, 4), 8, 8,
		9, 8, rep(6, 5), NA, 8,
		9, 7, rep(5, 5), NA, 7,
		7, 7, rep(5, 4), rep(7, 12))
	vf <= criteria
}

agis.clusters <- function(vf)
# return indices of clusters seperately for upper and lower hemifields and nasal sector
{
	abn <- agis.is.abnormal(vf)
	clusterize <- function(clusterlist, unassigned)
	{
		if(length(unassigned) == 0)
			clusterlist
		else
		{
			if(length(clusterlist)==0)
				clusterize(list(unassigned[1]), unassigned[-1])
			else
			{
				cc = clusterlist[[length(clusterlist)]]
				neighbors = unique(do.call(c, agis.neighbors[cc]))
				newindices = unassigned %in% neighbors
				newmembers = unassigned[newindices]
				if(length(newmembers) == 0)
				{
					clusterlist[[length(clusterlist)+1]] = unassigned[1]
					clusterize(clusterlist, unassigned[-1])
				}
				else
				{
					newunassigned = unassigned[!newindices]
					clusterlist[[length(clusterlist)]] = sort(c(cc, newmembers))
					clusterize(clusterlist, newunassigned)
				}
			}
		}
	}
	upperind = which(agis.vf.sectors == "upper" & abn)
	clusters.upper = clusterize(list(), upperind)
	lowerind = which(agis.vf.sectors == "lower" & abn)
	clusters.lower = clusterize(list(), lowerind)
	nasalind = which(agis.vf.sectors == "nasal" & abn)
	clusters.nasal = clusterize(list(), nasalind)
	list(upper = clusters.upper, lower = clusters.lower, nasal = clusters.nasal)
}

agis.score <- function(tds)
# according to  AGIS 2 (Gaasterland et al., 1994), p. 1448, and
# Katz (1999), p. 392
{
	n = length(tds)
	if(n<52)
		stop("agis.score: too few elements in TD vector (MUST be 52 or 54)")
	if(n>54)
	{
		# try to extract TDs from the data structure:
		if("td1" %in% names(tds))
			tds <- tds[grep("^td[0-9]+", names(tds))]
		else
			stop("agis.score: too many elements in TD vector (MUST be 52 or 54, or a data structure that contains names td1, ..., td54)")
	}

	vf <- if(length(tds)<54) c(tds[1:25], NA, tds[26:33], NA, tds[34:52])  else tds
	cl <- agis.clusters(vf)
	score = 0
	# nasal:
	if(length(cl$nasal) > 0)
	{
		if(length(cl$nasal) == 1 && length(cl$nasal[[1]]) < 3) # nasal step if it's restricted to one hemifield:
		{
			if(all(cl$nasal[[1]] %in% c(11, 19, 20)) || all(cl$nasal[[1]] %in% c(28, 29, 37)))
				score = score+1
		} else # "nasal defect"
			score = score+ifelse(any(sapply(cl$nasal, length) > 2), 1, 0)

		lessequal12 <- which(vf[which(agis.vf.sectors == "nasal")] <= -12)
		if(length(lessequal12)>=4) score = score+1
	}
	# hemifields:
	score.locations <- function(clusterlist)
	{
		score = 0
		number.per.cluster <- sapply(clusterlist, length)
		# only clusters >= 3:
		greatereq3 <- which(number.per.cluster>=3)
		if(length(greatereq3) > 0)
		{
			s = sum(number.per.cluster[greatereq3])
			if(s>=3) score = score+1
			if(s>=6) score = score+1
			if(s>=13) score = score+1
			if(s>=20) score = score+1
			# add even more if half of the locations exceed a certain value:
			loc3 <- vf[ do.call(c, clusterlist[greatereq3]) ]
			l3h <- length(loc3)/2
			addone <- function(criterion)
				ifelse(length(which(loc3<=-criterion)) >= l3h, 1, 0)
			score = score+addone(12)
			score = score+addone(16)
			score = score+addone(20)
			score = score+addone(24)
			score = score+addone(28)
		}
		score
	}
	score <- score + score.locations(cl$upper)
	score <- score + score.locations(cl$lower)
	score
}

#===========================================================================#
#### CIGTS VF scoring (Musch et al., 1999; Gillespie et al., 2003)

# neighboring VF locations within each hemifield,
# according to Gillespie et al. (2003), with 52 indices
cigts.neighbors <- list(
	c(2, 5, 6, 7), c(1, 3, 6:8), c(2, 4, 7:9), c(3, 8:10),
	c(1, 6, 11:13), c(1, 2, 5, 7, 12:14), c(1:3, 6, 8, 13:15), c(2:4, 7, 9, 14:16), c(3, 4, 8, 10, 15:17), c(4, 9, 16:18),
	c(5, 12, 19:21), c(5, 6, 11, 13, 20:22), c(5:7, 12, 14, 21:23), c(6:8, 13, 15, 22:24), c(7:9, 14, 16, 23:25), c(8:10, 15, 17, 24, 25), c(9, 10, 16, 18, 25, 26), c(10, 17, 26),
	c(11, 20), c(11, 12, 19, 21), c(11:13, 20, 22), c(12:14, 21, 23), c(13:15, 22, 24), c(14:16, 23, 25), c(15:17, 24), c(17, 18),
	c(28, 35), c(27, 29, 35, 36), c(28, 30, 35:37), c(29, 31, 36:38), c(30, 32, 37:39), c(31, 33, 38:40), c(32, 39:41), c(41, 42),
	c(27:29, 36, 43), c(28:30, 35, 37, 43, 44), c(29:31, 36, 38, 43:45), c(30:32, 37, 39, 44:46), c(31:33, 38, 40, 45:47), c(32, 33, 39, 41, 46:48), c(33, 34, 40, 42, 47, 48), c(34, 41, 48),
	c(35:37, 44, 49), c(36:38, 43, 45, 49, 50), c(37:39, 44, 46, 49:51), c(38:40, 45, 47, 50:52), c(39:41, 46, 48, 51, 52), c(40:42, 47, 52),
	c(43:45, 50), c(44:46, 49, 51), c(45:47, 50, 52), c(46:48, 51))

cigts.score <- function(tdprobs)
# CIGTS VF scoring (Gillespie et al., 2003)
# tdprobs: vector of length 52 representing the TD probabilities (between 0.005 and 1)
# example from Gillespie:
#    tdprobs = c(0.05, 0.02, 0.05, rep(1,8), 0.05, 0.02, rep(1,7), 0.01, 0.005, rep(1,6), 0.02, rep(1,17), 0.01, rep(1,5))
#    cigts.score(tdprobs) -> 0.7692308
{
	n = length(tdprobs)
	if(n<52)
		stop("cigts.score: too few elements in TD prob vector (MUST be 52 or 54)")
	if(n>54)
	{
		# try to extract TDs from the data structure:
		if("tdp1" %in% names(tdprobs))
			tdprobs <- tdprobs[grep("^tdp[0-9]+", names(tdprobs))]
		else
			stop("cigts.score: too many elements in TD prob vector (MUST be 52 or 54, or a data structure that contains names td1, ..., td54)")
	}
	if(n==54)
		tdprobs <- tdprobs[-c(26,35)]

	pweights <- ifelse(tdprobs==0.005, 4, ifelse(tdprobs==0.01, 3, ifelse(tdprobs==0.02, 2, ifelse(tdprobs==0.05, 1, 0))))
	# calculate the weights relevant for scoring:
	get.effective.weight <- function(weight, neighbors)
	{
		neighborweights = sort(pweights[neighbors], decreasing=T)
		min(neighborweights[2], weight)
	}
	effective.weights <- mapply(get.effective.weight, pweights, cigts.neighbors)
	sum(effective.weights)/10.4
}

##### input quaity check, cigts:

input.check.cigts <- function (measmatrix)
{
  n = nrow(measmatrix)
  if(n < 5)
    stop("progression.cigts: at least 5 VFs required")

  n.tdprobs = length(grep("^tdp[0-9]+", colnames(measmatrix)))

  if(n.tdprobs < 52)
  {stop("progression.cigts: too few elements in TD probability (tdp) vector (MUST be 52 or 54. The data structure should contain names 'tdp1', ..., 'tdp52' or until 'tdp54')")}

  if(n.tdprobs == 52)
    if("tdp1" %in% names(measmatrix))
    {tdprobs <- measmatrix[ , grep("^tdp[0-9]+", colnames(measmatrix))]}
  else
  {stop("progression.cigts: too few elements in TD probability (tdp) vector (MUST be 52 or 54. The data structure should contain names 'tdp1', ..., 'tdp52' or until 'tdp54')")}

  if(n.tdprobs == 54)
    tdprobs <- tdprobs[-c(26,35)]

  if(n.tdprobs > 54)
  {
    # try to extract TDs from the data structure:
    stop("progression.cigts: too many elements in TD probability (tdp) TD vector (MUST be 52 or 54. The data structure should contain names 'tdp1', ..., 'tdp52' or until 'tdp54')")
  }

  return (as.matrix(tdprobs))
}

#===================================#
#base function, cigts
progression.cigts.base <- function(measmatrix)
# CIGTS VF progression (Musch et al., 1999)
# measmatrix: columns MUST contain the 52 TD probs and yearsfollowed,
#    rows represent the single measurements
# returns "stable", "worsening", or "improving"
# note: If a VF series is temporarily improving and
#    temporarily worsening, it is assumed to be "stable" overall
{
  tdprobs = input.check.cigts(measmatrix)
  # tdprobs = measmatrix[, grep("^tdp[0-9]+", colnames(measmatrix))]
	cigts.scores = apply(tdprobs, 1, cigts.score)
	baseline = mean(cigts.scores[1:2])

	tl = rev(cigts.scores[-(1:2)] - baseline)
	results = ifelse(tl >= 3, "worsening", ifelse(tl <= -3, "improving", "stable"))
	final = unique(results[1:3])

	if(length(final) == 1)
	{
		inter = results[-(1:3)]
		# if ever a VF series is both "improving" and "worsening" for single VFs, we assume it to be stable overall
		ifelse(any(inter != "stable" & inter != final), "stable", final)
	} else "stable"
}


#' CIGTS VF progression
#'
#' \code{progression.cigts} returns the progression of visual field test based on 52 or 54 total deviation probabilities (tdp). CIGTS VF progression (Musch et al., 1999).
#' @param measmatrix is a data frame. MUST contain the following columns: 52/54 TD probs (column names MUST be 'tdp1' ~ 'tdp52' or 'tdp1' ~ 'tdp54'), 'yearsfollowed', and 'eyeid'. Rows represent the single measurements. The minimum measurements (rows) is 5.
#' @return "stable", "worsening", or "improving" of measurements in \code{measmatrix}. Note: If a VF series is temporarily improving and temporarily worsening, it is assumed to be "stable" overall
#' @references  \url{http://www.aaojournal.org/article/S0161-6420(99)90147-1/abstract}
#' @examples
#' data(vf.cigts)
#' colnames(vf.cigts)
#' progression.cigts(vf.cigts)
#' progression.cigts(vf.cigts[vf.cigts$eyeid == 1,])
#' progression.cigts(vf.cigts[vf.cigts$eyeid == 2,])
#' @importFrom stats lm
#' @export
#'

progression.cigts <- function(measmatrix)
{
  if(!("eyeid" %in% colnames(measmatrix)))
  {
    warning("progression.cigts: input does not contain column named 'eyeid'. Assuming that all measurements are from the same eye.")
    measmatrix$eyeid <- 1
  }

  method = 'cigts'

  do.call(
    "rbind",
    by(
      measmatrix,
      measmatrix$eyeid,
      function(eye)
        sapply(
          method,
          function(meth) do.call(paste("progression", meth, 'base', sep="."), list(eye))),  #Dian, added 'base' 5.16.2019
      simplify=F))

  # progression.cigts.base (measmatrix)
}
#===========================================================================#
##### input quaity check, schell2014:
input.check.schell2014 <- function (measmatrix)
{
  if(nrow(measmatrix)<4)
    stop("progression.schell2014: at least 4 VFs required")

  if (!('md' %in% colnames(measmatrix))) stop("progression.schell2014: Column with name 'md' missing")
}
#===================================#
#base function, schell2014
progression.schell2014.base <- function(measmatrix)
  # progression criterion after Schell et al. 2014
  # which is essentially like CIGTS but with MD, and only
  #    one follow-up is enough to confirm progression.
  # note: If a VF series is temporarily improving and
  #    temporarily worsening, it is assumed to be "stable" overall
{

  input.check.schell2014(measmatrix)

  mds = measmatrix[, "md"]
  baseline = mean(mds[1:2])
  tl = rev(mds[-(1:2)] - baseline)
  results = ifelse(tl <= -3, "worsening", ifelse(tl >= 3, "improving", "stable"))
  final = unique(results[1:2])

  if(length(final) == 1)
  {
    inter = results[-(1:2)]
    # if ever a VF series is both "improving" and "worsening" for single VFs, we assume it to be stable overall
    ifelse(any(inter != "stable" & inter != final), "stable", final)
  } else "stable"
}

##### Progression detection methods:

#' Schell 2014 VF progression
#'
#' \code{progression.schell2014} returns the progression criterion after Schell et al. 2014, which is essentially like CIGTS but with MD, and only one follow-up is enough to confirm progression.
#' @param measmatrix is a data frame. MUST contain the following columns: 'md' (mean deviation) and 'eyeid'. Rows represent the single measurements. The minimum measurements (rows) is 4.
#' @return "stable", "worsening", or "improving" of measurements in \code{measmatrix}.  Note: If a VF series is temporarily improving and temporarily worsening, it is assumed to be "stable" overall
#' @seealso \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4495761/}
#' @examples
#' data(vf.schell2014)
#' colnames(vf.schell2014)
#' progression.schell2014(vf.schell2014)
#' progression.schell2014(vf.schell2014[vf.schell2014$eyeid == 1,])
#' progression.schell2014(vf.schell2014[vf.schell2014$eyeid == 2,])
#' @importFrom stats lm
#' @export
#'

progression.schell2014 <- function(measmatrix)
{
  if(!("eyeid" %in% colnames(measmatrix)))
  {
    warning("progression.schell2014: input does not contain column named 'eyeid'. Assuming that all measurements are from the same eye.")
    measmatrix$eyeid <- 1
  }

  method = 'schell2014'

  do.call(
    "rbind",
    by(
      measmatrix,
      measmatrix$eyeid,
      function(eye)
        sapply(
          method,
          function(meth) do.call(paste("progression", meth, 'base', sep="."), list(eye))),  #Dian, added 'base' 5.16.2019
      simplify=F))
}
#===========================================================================#
### Pointwise Linear Regression (PLR) progression detection methods:

plr.two.omit <- function(timepoints, tds)
# Pointwise Linear Regression (PLR) progression detection method
# according to Gardiner & Crabb (2002);
# timepoints: measurement time, in years (either age or yearsfollowed)
# tds: vector of TDs at a single location over time
# returns "stable", "worsening", or "improving"
{
	n = length(timepoints)
	if(n<3) stop("plr.two.omit: at least 3 measurements required")

	standard.crit <- function(tpts, tdvals)
	{
		m = lm(tdvals ~ tpts)
		slope = m$coefficients[2]
		p = (summary(m))$coefficients[2,4]
		ifelse(abs(slope)>=1 && p <= 0.01, ifelse(slope<0, "worsening", "improving"), "stable")
	}
	#print(tds[-(n-1)])
	r1 = standard.crit(timepoints[-n], tds[-n])
	r2 = standard.crit(timepoints[-(n-1)], tds[-(n-1)])
	#cat("r1 =",r1, ", r2 =", r2, " \n")
	ifelse(r1==r2, r1, "stable")
}


##### input quaity check, plr.nouri.2012:

input.check.plr.nouri.2012 <- function (measmatrix)
{
  if (!('yearsfollowed' %in% colnames(measmatrix))) stop("progression.plr.nouri.2012: Column with name 'yearsfollowed' missing")
  else {
    n = length(measmatrix[, "yearsfollowed"])
    if(n < 3)
      stop("progression.plr.two.omit: at least 3 measurements required")
  }

  n.tds = length(grep("^td[0-9]+", colnames(measmatrix)))

  if(n.tds < 52)
    {stop("progression.plr.nouri.2012: too few elements in TD vector (MUST be 52 or 54. The data structure should contain names 'td1', ..., 'td52' or until 'td54')")}

  if(n.tds == 52)
    if("td1" %in% names(measmatrix))
      {tds <- measmatrix[ , grep("^td[0-9]+", colnames(measmatrix))]}
    else
      {stop("progression.plr.nouri.2012: too few elements in TD vector (MUST be 52 or 54. The data structure should contain names 'td1', ..., 'td52' or until 'td54')")}

  if(n.tds == 54)
    tds <- tds[-c(26,35)]

  if(n.tds > 54)
  {
    # try to extract TDs from the data structure:
    stop("progression.plr.nouri.2012: too many elements in TD vector (MUST be 52 or 54. The data structure should contain names 'td1', ..., 'td52' or until 'td54')")
  }

  return (as.matrix(tds))
}
#===================================#
#base function, plr.nouri.2012
progression.plr.nouri.2012.base <- function(measmatrix)
  # Pointwise Linear Regression (PLR) progression detection method
  # according to Nouri-Mahdavi et al. (2012);
  # measmatrix: columns MUST contain the 52 TDs and yearsfollowed,
  #    rows represent the single measurements
  # returns "stable", "worsening", or "improving"
{
  # tds = measmatrix[, grep("^td[0-9]+", colnames(measmatrix))]
  tds = input.check.plr.nouri.2012(measmatrix)

  results = apply(
    tds,
    2,
    function(v) plr.two.omit(measmatrix[, "yearsfollowed"], v))

  worsening = sum(results=="worsening")
  improving = sum(results=="improving")
  #cat(improving, ",", worsening, "\n")
  ifelse(
    worsening >= improving + 3,
    "worsening",
    ifelse(improving >= worsening + 3, "improving", "stable"))
}

##### Progression detection methods:

#' Nouri-Mahdavi 2012 VF progression
#'
#' \code{progression.plr.nouri.2012} returns the progression criterion, using  Pointwise Linear Regression (PLR) progression detection method according to Nouri-Mahdavi et al. (2012).
#' @param measmatrix is a data frame. MUST contain the following columns: 52/54 TD (column names MUST be 'td1' ~ 'td52' or 'td1' ~ 'td54'), 'yearsfollowed', and 'eyeid'. Rows represent the single measurements. The minimum measurements (rows) is 3.
#' @return "stable", "worsening", or "improving" of measurements in \code{measmatrix}
#' @seealso \url{https://www.ncbi.nlm.nih.gov/pubmed/22427560/}
#' @examples
#' data(vf.plr.nouri.2012)
#' colnames(vf.plr.nouri.2012)
#' progression.plr.nouri.2012(vf.plr.nouri.2012)
#' progression.plr.nouri.2012(vf.plr.nouri.2012[vf.plr.nouri.2012$eyeid == 1,])
#' progression.plr.nouri.2012(vf.plr.nouri.2012[vf.plr.nouri.2012$eyeid == 2,])
#' @importFrom stats lm
#' @export
progression.plr.nouri.2012 <- function(measmatrix)
{
  if(!("eyeid" %in% colnames(measmatrix)))
  {
    warning("progression.vfi: input does not contain column named 'eyeid'. Assuming that all measurements are from the same eye.")
    measmatrix$eyeid <- 1
  }

  method = 'plr.nouri.2012'

  do.call(
    "rbind",
    by(
      measmatrix,
      measmatrix$eyeid,
      function(eye)
        sapply(
          method,
          function(meth) do.call(paste("progression", meth, 'base', sep="."), list(eye))),  #Dian, added 'base' 5.16.2019
      simplify=F))
}


#===========================================================================#
##### input quaity check, vfi:
input.check.vfi <- function (measmatrix)
{
  if (!('yearsfollowed' %in% colnames(measmatrix))) stop("progression.vfi: Column with name 'yearsfollowed' missing")
  if (!('vfi' %in% colnames(measmatrix))) stop("progression.vfi: Column with name 'vfi' missing")

  n = length(measmatrix[, "yearsfollowed"])
  if(n < 3)
    stop("progression.vfi: at least 3 measurements required")
}
#===================================#
#base function, vfi
progression.vfi.base <- function(measmatrix)
  # progression according to VFI (significant slope, p<=0.05)
  # used in Aptel et al. (2015);
  # if timepoints is a matrix the rows of which contain the measurements,
  #    VFIs and yearsfollowed are extracted from the matrix and vfis is ignored
  # returns "stable", "worsening", or "improving"
{
  input.check.vfi(measmatrix)
  yearsfollowed = measmatrix[, "yearsfollowed"]
  vfis = measmatrix[, "vfi"]

  m = lm(vfis ~ yearsfollowed)
  slope = m$coefficients[2]
  pval = (summary(m))$coefficients[2,4]
  ifelse(!is.na(pval) && pval <= 0.05, ifelse(slope<0, "worsening", "improving"), "stable")
}
##### Progression detection methods:

#' progression according to VFI (significant slope, p<=0.05)
#'
#' \code{progression.vfi} returns the progression criterion used in Aptel et al. (2015).
#' @param measmatrix is a data frame. MUST contain the following columns: 'vfi' (visual field index), 'yearsfollowed', and 'eyeid'. Rows represent the single measurements. The minimum measurements (rows) is 3.
#' @return "stable", "worsening", or "improving" of measurements in \code{timepoints}
#' @seealso \url{https://www.ncbi.nlm.nih.gov/pubmed/26095771/}
#' @examples
#' data(vf.vfi)
#' colnames(vf.vfi)
#' progression.vfi(vf.vfi)
#' progression.vfi(vf.vfi[vf.vfi$eyeid == 1,])
#' progression.vfi(vf.vfi[vf.vfi$eyeid == 2,])
#' @importFrom stats lm
#' @export

progression.vfi <- function(measmatrix)
{
  if(!("eyeid" %in% colnames(measmatrix)))
  {
    warning("progression.vfi: input does not contain column named 'eyeid'. Assuming that all measurements are from the same eye.")
    measmatrix$eyeid <- 1
  }

  method = 'vfi'

  do.call(
    "rbind",
    by(
      measmatrix,
      measmatrix$eyeid,
      function(eye)
        sapply(
          method,
          function(meth) do.call(paste("progression", meth, 'base', sep="."), list(eye))),  #Dian, added 'base' 5.16.2019
      simplify=F))
}



#===========================================================================#

### general progression function:
#' general progression function
#'
#' \code{progression} returns the progression criterion with four methods. plr.nouri.2012, vfi, schell2014, cigts
#' @param vfseries is a data frame. MUST contain the following columns: yearsfollowed', and 'eyeid'. Rows represent the single measurements. Other requirements, such as number of minimum measurements (rows), and necessary VF measurements could be found in each progression method's documentation
#' @param method selected from one or more from: plr.nouri.2012, vfi, schell2014, cigts. Default it ...
#' @return "stable", "worsening", or "improving" of measurements in \code{measmatrix}
#' @seealso \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4495761/}
#' @examples
#' data(vfseries)
#' progression(vfseries)
#' progression(vfseries[vfseries$eyeid == 1,])
#' progression(vfseries[vfseries$eyeid == 2,])
#' progression(vfseries, method=c("cigts"))
#' @export
progression <- function(vfseries, method=c("plr.nouri.2012", "vfi", "schell2014", "cigts"))
{
	if(is.matrix(vfseries))
		vfseries <- as.data.frame(vfseries)
	if(!is.data.frame(vfseries))
		stop("progression: first argument MUST be matrix or data frame.")
	if(!("yearsfollowed" %in% colnames(vfseries)))
	{
		if("age" %in% colnames(vfseries))
			vfseries$yearsfollowed <- vfseries$age - vfseries$age[1]
		else
			stop("progression: first argument does not contain columns named yearsfollowed or age.")
	}
	if(!("eyeid" %in% colnames(vfseries)))
	{
		warning("progression: first argument does not contain column named eyeid. Assuming that all measurements are from the same eye.")
		vfseries$eyeid <- 1
	}

	do.call(
		"rbind",
		by(
			vfseries,
			vfseries$eyeid,
			function(eye)
				sapply(
					method,
					function(meth) do.call(paste("progression", meth, 'base', sep="."), list(eye))),  #Dian, added 'base' 5.16.2019
				simplify=F))

}




linear.md <- function(tdvec, capped=FALSE)
# calculate a "more linear" version of MD from TDs
# by linearizing TDs, as described in Gardiner et al. (2014, p. 545)
# capped: all TDs are capped at zero
{
	if(capped)
		tdvec <- sapply(tdvec, function(x) min(x, 0))

	mean(10**(tdvec/10))
}



