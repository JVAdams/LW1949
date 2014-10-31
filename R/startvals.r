#' Determine Starting Values for LC50\% and LC99.9\%
#'
#' Determine starting values for the LC50\% and LC99.9\% (both on the log10 scale) using simple linear regression on the log10 dose vs. probit effect scale.
#' @param dat 	A data frame of raw toxicity data, including these two variables:
#'	ldose = dose (the concentration of the applied chemical on the log10 scale), 
#'	and pbpdead, the proportion of dead individuals (on the probit scale, with 0s converted to 0.1\% and 1s converted to 99.9\%).
#' @param fit	A model object that can be used to predict the corrected values (as proportions) from \code{distexpprop5}, 
#'	the distance from the expected values (as proportions) and 0.5.  Typically the output from \code{\link{gamtable1}()}.
#' @return 			A list with two elements: sv, a numeric vector of length two giving the starting values for the LC50\% and LC99.9\%,
#'	and p, a numeric scalar giving the P value of the associated chi-squared statistic.
#' @export
#' @examples 
#' test <- data.frame(
#' 	dose=c(0.0625, 0.125, 0.25, 0.5), 
#' 	ntot=rep(8, 4), 
#' 	ndead = c(1, 4, 4, 7))
#' test$pdead = test$ndead/test$ntot
#' test$mcat = mcat(test)
#' test$ldose <- log10(test$dose)
#' test$pbpdead <- probit(test$pdead)
#' gamfit <- gamtable1()
#' startvals(test, gamfit)

startvals <- function(dat, fit) {
	# define log10(lc50) and log10(lc999) starting values
	slr <- lm(pbpdead ~ ldose, data=dat)
	lc50.1 <- as.numeric(-slr$coef[1]/slr$coef[2])
	lc999.1 <- as.numeric((probit(0.999) - slr$coef[1])/slr$coef[2])
	sv <- c("log10LC50"=lc50.1, "log10LC99.9"=lc999.1)
	p <- assessfit(LCs=sv, dat=dat, fit)
	list(start=sv, pval=p)
	}
