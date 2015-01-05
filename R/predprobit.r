#' Determine the Effective Dose from a Probit Regression Fit
#'
#' Determine the effective dose for a specified percent effect from a fitted probit regression model.
#' @param pct 	A numeric scalar of the effect (as a percent) for which to estimate the effective dose.
#' @param pfit 	An object of class \code{\link{glm}} representing a probit regression fit to dose-effect data, 
#'	typically the result of a call to \code{\link{fitprobit}}.
#' @return 		A numeric vector of length three, the effective dose and the lower and upper 95\% confidence limits.
#' @import		MASS
#' @export
#' @examples 
#' conc <- c(0.0625, 0.125, 0.25, 0.5, 1)
#' numtested <- rep(8, 5)
#' nalive <- c(1, 4, 4, 7, 8)
#' mydat <- dataprep(dose=conc, ntot=numtested, nfx=nalive)
#' myfit <- fitprobit(mydat)
#' predprobit(50, myfit)

predprobit <- function(pct, pfit) {
	if(pfit$converged) {
		logED <- dose.p(pfit, cf=1:2, p=pct/100)
		ED <- 10^c(as.numeric(logED) + c(0, -1.96, 1.96) * as.numeric(attr(logED, "SE")))
		} else {
		ED <- rep(NA, 3)
		}
	names(ED) <- c("ED", "lower", "upper")
	ED
	}
