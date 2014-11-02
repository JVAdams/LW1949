#' Chi-Squared Statistic
#'
#' Calculate the chi-squared statistic from observed and expected counts.
#' @param obsn 	A numeric vector of observed counts.
#' @param expn 	A numeric vector of expected counts.
#' @return 		A list of length two.  The first element is a numeric vector of length three: chistat, the chi-squared statistic; 
#'	df, the degrees of freedom of \code{chistat}; and pval, their associated P value.
#'	The second element is a numeric vector the same length as \code{obsn}, contributions to the chi-squared.
#' @export
#' @seealso 	\code{\link{chisq.test}}.
#' @examples 
#' chi2(c(10, 8, 3), c(7, 7, 7))

chi2 <- function(obsn, expn) {
	# chi squared value from observed and expected numbers
	contrib <- (obsn - expn)^2 / expn
	chistat <- sum(contrib)
	df <- length(obsn) - 2
	df[df<0] <- NA
	pval <- NA
	pval[length(obsn)> 2.5] <- 1 - pchisq(chistat, df)
	list(chi=c(chistat=chistat, df=df, pval=pval), contrib=contrib)
	}
