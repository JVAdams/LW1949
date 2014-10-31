#' Chi-Squared Statistic
#'
#' Calculate the chi-squared statistic from observed and expected counts.
#' @param obsn 	A numeric vector of observed counts.
#' @param expn 	A numeric vector of expected counts.
#' @return 		A numeric vector of length three: chistat, the chi-squared statistic; 
#'	pval, its associated P value; and df, the degrees of freedom of \code{chistat}.
#' @export
#' @seealso 	\code{\link{chisq.test}}.
#' @examples 
#' chi2(c(10, 8, 3), c(7, 7, 7))

chi2 <- function(obsn, expn) {
	# chi squared value from observed and expected numbers
	chistat <- sum((obsn - expn)^2 / (expn))
	df <- length(obsn) - 2
	pval <- 1 - pchisq(chistat, df)
	c(chistat=chistat, pval=pval, df=df)
	}
