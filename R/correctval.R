#' Predict the corrected proportion using a model fit of Table 1 of Litchfield
#'   and Wilcoxon (1949)
#'
#' Given an expected proportion, calculate the corrected proportion using a
#'   model fit of Table 1 of Litchfield and Wilcoxon (1949).
#' @param val
#'   A numeric vector of expected values (as proportions).
#' @param fit
#'   A model object that can be used to predict the corrected values
#'     (as proportions) from \code{distexpprop5}, the distance from the expected
#'     values (as proportions) and 0.5.  Typically the output from
#'     \code{\link{gamtable1}()}.
#' @return
#'   A numeric vector of corrected values (as proportions),
#'     the same length as \code{val}.
#' @import
#'   mgcv
#' @export
#' @references Litchfield, JT Jr. and F Wilcoxon.  1949.
#' A simplified method of evaluating dose-effect experiments.
#' Journal of Pharmacology and Experimental Therapeutics 99(2):99-113.
#' \href{http://jpet.aspetjournals.org/content/96/2/99.abstract}{[link]}.
#' @examples
#' gamfit <- gamtable1()
#' correctval(c(0.37, 0.5, 0.63), gamfit)

correctval <- function(val, fit) {
	correction <- predict(fit, newdata=data.frame(distexpprop5 = abs(0.5 - val)))
	result1 <- ifelse(val < 0.5, correction, 1-correction)
	result2 <- ifelse(val >= 0.01 & val <= 0.99, result1, val)
	result2
	}
