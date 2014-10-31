#' Chi-Squared Statistic
#'
#' Calculate the chi-squared statistic from observed and expected counts.
#' @param obsn 	A numeric vector of observed counts.
#' @param expn 	A numeric vector of expected counts.
#' @return 		A list with three elements: chistat, a numeric scalar, the chi-squared statistic; 
#'	pval, a numeric scalar, its associated P value; and df, an integer, the degrees of freedom of \code{chistat}.
#' @export
#' @seealso 	\code{\link{chisq.test}}.
#' @examples 
#' chi2(c(10, 8, 3), c(7, 7, 7))

chi2 <- function(obsn, expn) {
	# chi squared value from observed and expected numbers
	chistat <- sum((obsn - expn)^2 / (expn))
	df <- length(obsn) - 2
	pval <- 1 - pchisq(chistat, df)
	list(chistat=chistat, pval=pval, df=df)
	}
