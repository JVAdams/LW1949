#' Assess Fit of Dose Response Curve
#'
#' Assess the fit of a dose response curve using the chi-squared statistic.
#' The curve is described by the intercept and slope of a straight line in the log dose vs. probit effect scale.
#' @param params	A numeric vector of length two, with the estimated intercept and slope of the dose-effect relation on the
#'	log10 and probit scale.	These parameters define the dose response curve.
#' @param DEdata 	A data frame of dose-effect data (typically, the output from \code{\link{dataprep}})
#'	containing at least these four variables: dose, ntot, pfx, fxcateg.
#' @param fit	A model object that can be used to predict the corrected values (as proportions) from \code{distexpprop5}, 
#'	the distance from the expected values (as proportions) and 0.5.  Typically the output from \code{\link{gamtable1}()}.
#' @param simple	A logical scalar indicating if the output should be restricted just the P value, default TRUE.
#' @return 			If \code{simple=FALSE}, a list of length two.  The first element, \code{chi}, is a numeric vector of length three: 
#'	chistat, the chi-squared statistic; df, the degrees of freedom of \code{chistat}; and pval, their associated P value.
#'	The second element, \code{stepB}, is a matrix of three numeric vectors the same length as \code{obsn}, 
#'	exp, expected effects; expcorr, expected effects corrected; and contrib, contributions to the chi-squared.
#'	If \code{simple=TRUE}, a numeric scalar, the chi-squared statistic (see details).
#' @export
#' @details			This function is used as part of a routine that attempts to find the dose response curve that minimizes the 
#'	chi-squared statistic measuring the distance between the observed and expected values of the proportion affected.  
#' 	Following Litchfield and Wilcoxon (1949, steps B1 and B2), 
#'  records for any 0\% or 100\% dose with expected values < 0.01\% or > 99.99\% are deleted,
#'	and expected values are corrected using the \code{\link{correctval}} function.
#' @seealso 		\code{\link{chi2}} and \code{\link{chisq.test}}.
#' @references Litchfield, JT Jr. and F Wilcoxon.  1949.
#' A simplified method of evaluating dose-effect experiments. 
#' Journal of Pharmacology and Experimental Therapeutics 99(2):99-113.
#' \href{http://jpet.aspetjournals.org/content/96/2/99.abstract}{[link]}. 
#' @examples 
#' conc <- c(0.0625, 0.125, 0.25, 0.5, 1)
#' numtested <- rep(8, 5)
#' nalive <- c(1, 4, 4, 7, 8)
#' mydat <- dataprep(dose=conc, ntot=numtested, nfx=nalive)
#' gamfit <- gamtable1()
#' assessfit(log10(c(0.125, 0.5)), mydat, gamfit, simple=FALSE)

assessfit <- function(params, DEdata, fit, simple=TRUE) {
	# calculate chi squared value from given line
	expected <- invprobit(params[1] + params[2]*log10(DEdata$dose))
	### B1. If the expected value for any 0% or 100% dose is < 0.01% or > 99.99%, delete record
	sel <- (expected >= 0.0001 & expected <= 0.9999) | DEdata$fxcateg==50
	n <- sum(sel)
	### B2. Using the expected effect, record a corrected value for each 0 and 100% effect
	cor.exp <- ifelse(DEdata$fxcateg==50, expected, correctval(expected, fit))
	### C. The chi squared test
	if(n < 0.5) {
		chilist <- list(chi=c(chistat=NA, df=NA, pval=NA), contrib=NA)
		} else {
		chilist <- chi2((DEdata$pfx*DEdata$ntot)[sel], (cor.exp*DEdata$ntot)[sel])
		}
	# expand contributions to chi-squared to original length
	stepB <- matrix(NA, nrow=length(expected), ncol=3, dimnames=list(NULL, c("exp", "expcorr", "contrib")))
	stepB[, "exp"] <- expected
	stepB[, "expcorr"] <- cor.exp
	stepB[sel, "contrib"] <- chilist$contrib
	if(simple) y <- chilist$chi["chistat"] else y <- list(chi=chilist$chi, stepB=stepB)
	y
	}
