#' Determine Linear Regression Coefficients from Dose-Effect Data
#'
#' Determine coefficients (intercept and slope) from dose-effect data using simple linear regression 
#'	on the log10 dose vs. probit effect scale.
#' @param DEdata 	A data frame of dose-effect data (typically, the output from \code{\link{dataprep}}
#'	containing eight variables: dose, ntot, nfx, pfx, logdose, bitpfx, fxcateg, and LWkeep.
#' @param fit		A model object that can be used to predict the corrected values (as proportions) from \code{distexpprop5}, 
#'	the distance from the expected values (as proportions) and 0.5.  Typically the output from \code{\link{gamtable1}()}.
#' @param constr	A numeric vector of length two, indicating the constraints (see \code{\link{constrain}})
#'	applied to the proportional effects, default c(0.0001, 0.9999).
#' @return 			A numeric vector of length two, the estimated intercept and slope.
#' @export
#' @examples 
#' conc <- c(0.0625, 0.125, 0.25, 0.5, 1)
#' numtested <- rep(8, 5)
#' nalive <- c(1, 4, 4, 7, 8)
#' mydat <- dataprep(dose=conc, ntot=numtested, nfx=nalive)
#' gamfit <- gamtable1()
#' DEcoef(mydat, gamfit)

DEcoef <- function(DEdata, fit, constr=c(0.0001, 0.9999)) {
	# define log10(lc50) and log10(lc999) starting values
	cbitpfx <- constrain(DEdata$bitpfx, probit(constr))
	lm(cbitpfx ~ logdose, data=DEdata[DEdata$LWkeep, ])$coef
	}
