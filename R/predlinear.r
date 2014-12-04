#' Determine the Effective Dose from a Linear Regression Fit
#'
#' Determine the effective dose for a specified percent effect from the intercept and slope of a linear regression.
#' @param pct 		A numeric vector of effects (in percents) for which to estimate the effective dose(s).
#' @param b0 		A numeric vector (more commonly a scalar) giving the intercept of the dose-response curve.
#' @param b1 		A numeric vector (more commonly a scalar) giving the slope of the dose-response curve.
#'	x = dose (the concentration of the applied chemical on the log10 scale), 
#'	and y, the proportion of affected individuals (on the probit scale, with 0s converted to 0.1\% and 1s converted to 99.9\%).
#' @return 			A numeric vector the same length as \code{pct} giving the estimated dose at the specified percent effect.
#' @export
#' @examples 
#' predlinear(c(16, 50, 84, 99.9), 1.700875, 2.199559)

predlinear <- function(pct, b0, b1) {
	ED <- as.numeric(10^( (probit(pct/100) - b0) / b1 ))
	names(ED) <- paste0("ED", pct)
	ED
	}
