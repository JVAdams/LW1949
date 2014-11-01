#' Determine Starting Values for LC50\% and LC99.9\%
#'
#' Determine starting values for the LC50\% and LC99.9\% (both on the log10 scale) using simple linear regression 
#'	on the log10 dose vs. probit effect scale.
#' @param DEdata 	A data frame of dose-effect data (typically, the output from \code{\link{dataprep}}
#'	containing eight variables: dose, ntot, ndead, pdead, logdose, bitpdead, mcateg, and LWkeep.
#' @param fit		A model object that can be used to predict the corrected values (as proportions) from \code{distexpprop5}, 
#'	the distance from the expected values (as proportions) and 0.5.  Typically the output from \code{\link{gamtable1}()}.
#' @param constr	A numeric vector of length two, indicating the constraints (see \code{\link{constrain}})
#'	applied to the proportional mortalities, default c(0.0001, 0.9999).
#' @return 			A list with two elements: sv, a numeric vector of length two giving the starting values for the LC50\% and LC99.9\%,
#'	and p, a numeric scalar giving the P value of the associated chi-squared statistic.
#' @export
#' @examples 
#' conc <- c(0.0625, 0.125, 0.25, 0.5, 1)
#' numtested <- rep(8, 5)
#' nmort <- c(1, 4, 4, 7, 8)
#' mydat <- dataprep(dose=conc, ntot=numtested, ndead=nmort)
#' gamfit <- gamtable1()
#' startvals(mydat, gamfit)

startvals <- function(DEdata, fit, constr=c(0.0001, 0.9999)) {
	# define log10(lc50) and log10(lc999) starting values
	cbitpdead <- constrain(DEdata$bitpdead, probit(constr))
	slr <- lm(cbitpdead ~ logdose, data=DEdata[DEdata$LWkeep, ])
	lc50.1 <- as.numeric(-slr$coef[1]/slr$coef[2])
	lc999.1 <- as.numeric((probit(0.999) - slr$coef[1])/slr$coef[2])
	sv <- c("log10LC50"=lc50.1, "log10LC99.9"=lc999.1)
	p <- assessfit(LCs=sv, DEdata=DEdata, fit)
	list(start=sv, pval=p)
	}
