#' Assess Fit of Dose Response Curve
#'
#' Assess the fit of a dose response curve using the chi-squared statistic.
#' The curve is described by two lethal concentrations, and is represented by a straight line in the log dose vs. probit effect scale.
#' @param LCs 		A numeric vector of estimated lethal concentrations on the log10 scale.  
#'	The first element is the LC50\%, the second is the LC99.9\%.	These two points define the dose response curve.
#' @param DEdata 	A data frame of dose-effect data (typically, the output from \code{\link{dataprep}}
#'	containing at least these four variables: dose, ntot, pdead, mcateg.
#' @param fit	A model object that can be used to predict the corrected values (as proportions) from \code{distexpprop5}, 
#'	the distance from the expected values (as proportions) and 0.5.  Typically the output from \code{\link{gamtable1}()}.
#' @param simple	A logical scalar indicating if the output should be restricted just the P value, default TRUE.
#' @return 			If \code{simple=FALSE}, a vector of length three: chistat, a numeric scalar, the chi-squared statistic; 
#'	pval, a numeric scalar, its associated P value; and df, an integer, the degrees of freedom of \code{chistat}.
#'	If \code{simple=TRUE}, a numeric scalar, the P value of the chi-squared statistic (see details).
#' @export
#' @details			This function is used as part of a routine that attempts to find the dose response curve that maximizes the 
#'	P value from the chi-squared statistic measuring the distance between the observed and expected values.  
#' 	Following Litchfield and Wilcoxon (1949, steps B1 and B2), 
#'  records for any 0\% or 100\% dose with expected values < 0.01\% or > 99.99\% are deleted,
#'	and expected values are corrected using the \code{\link{correctval}} function.
#' @seealso 		\code{\link{chi2}} and \code{\link{chisq.test}}.
#' @references J. T. Litchfield, Jr. and F. Wilcoxon.  1949. 
#' \href{http://jpet.aspetjournals.org/content/96/2/99.short}{A simplified method of evaluating dose-effect experiments}.
#' Journal of Pharmacology and Experimental Therapeutics 99(2):99-113.
#' @examples 
#' conc <- c(0.0625, 0.125, 0.25, 0.5, 1)
#' numtested <- rep(8, 5)
#' nmort <- c(1, 4, 4, 7, 8)
#' mydat <- dataprep(dose=conc, ntot=numtested, ndead=nmort)
#' gamfit <- gamtable1()
#' assessfit(log10(c(0.125, 0.5)), mydat, gamfit, simple=FALSE)

assessfit <- function(LCs, DEdata, fit, simple=TRUE) {
	# calculate chi squared value from given line (defined by log10 transform of the lc50 and lc999)
	LC50 <- LCs[1]
	LC999 <- LCs[2]
	slope <- probit(0.999) / (LC999 - LC50)
	int <- -slope*LC50
	expected <- invprobit(int + slope*log10(DEdata$dose))
	# B. 1. If the expected value for any 0% or 100% dose is < 0.01% or > 99.99%, delete record
	sel <- (expected >= 0.0001 & expected <= 0.9999) | DEdata$mcateg==50
	n <- sum(sel)
	# B. 2. Using the expected effect, record a corrected value for each 0 and 100% effect
	cor.exp <- ifelse(DEdata$mcateg==50, expected, correctval(expected, fit))
	if(n > 2) {
		### C. The chi squared test
		chilist <- chi2((DEdata$pdead*DEdata$ntot)[sel], (cor.exp*DEdata$ntot)[sel])
		} else {
		chilist <- c(chistat=NA, pval=NA, df=NA)
		}
	# save the p value
	if(simple) y <- chilist["pval"] else y <- chilist
	y
	}
