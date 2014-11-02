#' Determine the Lethal Concentration
#'
#' Determine the lethal concentration for a specified percent effect.
#' @param pct 		A numeric vector of effects (in percents) for which to estimate the lethal concentration.
#' @param b0 		A numeric vector (more commonly a scalar) giving the intercept of the dose-response curve.
#' @param b1 		A numeric vector (more commonly a scalar) giving the slope of the dose-response curve.
#'	x = dose (the concentration of the applied chemical on the log10 scale), 
#'	and y, the proportion of affected individuals (on the probit scale, with 0s converted to 0.1\% and 1s converted to 99.9\%).
#' @return 			A list with two elements: sv, a numeric vector of length two giving the starting values for the LC50\% and LC99.9\%,
#'	and p, a numeric scalar giving the P value of the associated chi-squared statistic.
#' @export
#' @examples 
#' LC(c(16, 50, 84, 99.9), 1.700875, 2.199559)

LC <- function(pct, b0, b1) {
	as.numeric(10^( (probit(pct/100) - b0) / b1 ))
	}
