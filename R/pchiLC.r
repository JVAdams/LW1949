#' P Value of a Chi-Squared Statistic for a Does Response Curve
#'
#' Calculate the P value of the chi-squared statistic for a given dose response curve relating log dose to proportion affected on the 
#'	probit scale.
#' @param LCs 		A numeric vector of estimated lethal concentrations on the log10 scale.  
#'	The first element is the LC50\%, the second is the LC99.9\%.	These two points define the does response curve.
#' @param dat 		A data frame of raw toxicity data, including these four variables:
#'	dose (the concentration of the applied chemical), ntot (the number of individuals tested), pdead (the proportion of dead individuals), 
#'	and mcat (mortality category =0 for none dead, =100 for all dead, and =50 for any partial mortality).
#' @param fit	A model object that can be used to predict the corrected values (as proportions) from \code{distexpprop5}, 
#'	the distance from the expected values (as proportions) and 0.5.  Typically the output from \code{\link{gamtable1}()}.
#' @param outlist	A logical scalar indicating if the output should be a list or a scalar, default FALSE.
#' @return 			If \code{outlist=TRUE}, a list with three elements: chistat, a numeric scalar, the chi-squared statistic; 
#'	pval, a numeric scalar, its associated P value; and df, an integer, the degrees of freedom of \code{chistat}.
#'	If \code{outlist=FALSE}, a numeric scalar, the negative P value of the chi-squared statistic (see details).
#' @export
#' @details			This function is used as part of a routine that attempts to find the dose response curve that minimizes the 
#'	P value from the chi-squared statistic measuring the distance between the observed and expected values.  
#' 	Following Litchfield and Wilcoxon (1949), records for any 0\% or 100\% dose with expected values < 0.01\% or > 99.99\% are deleted,
#'	and expected values are corrected using the \code{\link{correctval}} function.
#' @seealso 		\code{\link{chi2}} and \code{\link{chisq.test}}.
#' @references J. T. Litchfield, Jr. and F. Wilcoxon.  1949. 
#' \href{http://jpet.aspetjournals.org/content/96/2/99.short}{A simplified method of evaluating dose-effect experiments}.
#' Journal of Pharmacology and Experimental Therapeutics 99(2):99-113.
#' @examples 
#' test <- data.frame(
#' 	dose=c(0.0625, 0.125, 0.25, 0.5), 
#' 	ntot=rep(8, 4), 
#' 	ndead = c(0, 4, 6, 8))
#' test$pdead <- test$ndead/test$ntot
#' test$mcat <- mcat(test)
#' gamfit <- gamtable1()
#' pchiLC(c(0.125, 0.5), test, gamfit)

pchiLC <- function(LCs, dat, fit, outlist=FALSE) {
	# calculate chi squared value from given line (defined by log10 transform of the lc50 and lc999)
	LC50 <- LCs[1]
	LC999 <- LCs[2]
	slope <- probit(0.999) / (LC999 - LC50)
	int <- -slope*LC50
	expected <- invprobit(int + slope*log10(dat$dose))
	# B. 1. If the expected value for any 0% or 100% dose is < 0.01% or > 99.99%, delete record
	sel <- (expected >= 0.0001 & expected <= 0.9999) | dat$mcat==50
	n <- sum(sel)
	# B. 2. Using the expected effect, record a corrected value for each 0 and 100% effect
	cor.exp <- ifelse(dat$mcat==50, expected, correctval(expected, fit))
	if(n > 2) {
		### C. The chi squared test
		chilist <- chi2((dat$pdead*dat$ntot)[sel], (cor.exp*dat$ntot)[sel])
		} else {
		chilist <- list(chistat=NA, pval=NA, df=NA)
		}
	# save the p value as a negative for minimization by optim()
	if(outlist) y <- chilist else y <- -chilist$pval
	y
	}
