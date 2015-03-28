#' Chi-Squared Statistic
#'
#' Calculate the chi-squared statistic from observed and expected counts.
#' @param obsn
#'   A numeric vector of observed counts.
#' @param expn
#'   A numeric vector of expected counts.
#' @return
#'   A list of length two.
#'   The first element is a numeric vector of length three:
#'     \code{chistat}, chi-squared statistic;
#'     \code{df}, degrees of freedom; and
#'     \code{pval}, P value.
#'	 The second element is a numeric vector the same length as \code{obsn},
#'     contributions to the chi-squared.
#' @export
#' @seealso
#'   \code{\link{chisq.test}}.
#' @examples
#' chi2(c(10, 8, 3), c(7, 7, 7))

chi2 <- function(obsn, expn) {
  if (!is.numeric(obsn) | !is.numeric(expn)) {
    stop("Inputs must be numeric.")
  }
  if (any((!is.na(obsn) & obsn < 0) | (!is.na(expn) & expn <= 0))) {
    stop("Inputs must be positive.")
  }
  sel <- !is.na(obsn) & !is.na(expn)
  if (sum(sel) < 1) stop("No non-missing data provided.")
  obsn <- obsn[sel]
  expn <- expn[sel]
	contrib <- (obsn - expn)^2 / expn
	chistat <- sum(contrib)
	df <- length(obsn) - 2
	df[df<0] <- NA
	pval <- NA
	pval[length(obsn)> 2.5] <- 1 - pchisq(chistat, df)
	list(chi=c(chistat=chistat, df=df, pval=pval), contrib=contrib)
}
