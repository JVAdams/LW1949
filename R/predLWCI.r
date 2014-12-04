#' Estimate the 95\% Confidence Interval of an Effective Dose from a Linear Regression Fit
#'
#' Estimate the 95\% confidence interval of an effective dose for a specified percent effect 
#'	from the intercept and slope of a linear regression using the approach of Litchfield and Wilcoxon (1949).
#' @param Y 	A numeric vector of effects (in percents) for which the effective dose confidence intervals are to be calculated.
#' @param EDY 	A numeric vector of effective dose(s) corresponding to \code{Y}.
#' @param fED50 A numeric vector (more commonly a scalar) giving the expansion factor(s) for the ED50
#'	of the dose-response curve (typically, output from \code{\link{fitLW}}.
#' @param fS 	A numeric vector (more commonly a scalar) giving the expansion factor(s) for the Litchfield and Wilcoxon (1949)
#'	slope (typically, output from \code{\link{fitLW}}.
#' @return 		An n*2 numeric matrix with the Litchfield and Wilcoxon (1949) lower and upper 95\% confidence intervals
#'	of the effective dose, where n is the length of \code{Y} and \code{EDY}.
#' @export
#' @details		Follows methods outlined in Litchfield and Wilcoxon (1949) on page 105, using equation 13 in the Appendix
#'	instead of Nomograph 4.
#' @references Litchfield, JT Jr. and F Wilcoxon.  1949.
#'	A simplified method of evaluating dose-effect experiments. 
#'	Journal of Pharmacology and Experimental Therapeutics 99(2):99-113.
#'	\href{http://jpet.aspetjournals.org/content/96/2/99.abstract}{[link]}. 
#' @examples 
#' dose <- c(0.0625, 0.125, 0.25, 0.5, 1)
#' ntested <- rep(8, 5)
#' nalive <- c(1, 4, 4, 7, 8)
#' mydat <- dataprep(dose=dose, ntot=ntested, nfx=nalive)
#' mydat
#' flw <- fitLW(mydat)
#' pcts <- c(25, 99.9)
#' EDYs <- predlinear(pcts, b0=flw$params[1], b1=flw$params[2])
#' predLWCI(Y=pcts, EDY=EDYs, fED50=flw$LWest["fED50"], fS=flw$LWest["fS"])

predLWCI <- function(Y, EDY, fED50, fS) {
	# X (Table 3)
	X <- abs(qnorm(Y/100))
	# fs^X (Nomograph 2)
	fSX <- fS^X
	# fEDY from equation 13 in Appendix (Nomograph 4)
	fEDY <- 10^sqrt( (log10(fSX))^2 + (log10(fED50))^2 )
	upperY <- EDY * fEDY
	lowerY <- EDY / fEDY
	cbind(lowerY, upperY)
	}
