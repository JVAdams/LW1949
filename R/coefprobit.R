#' Calculate the Coefficients of a Probit Regression Fit
#'
#' Calculate the coefficients from a fitted probit regression model.
#' @param pfit
#'   An object of class \code{\link{glm}} representing a probit regression fit
#'     to dose-effect data, typically the result of a call to
#'     \code{\link{fitprobit}}.
#' @return
#'   A numeric vector of length three, the intercept and slope of the
#'     dose-response curve, each with 95\% confidence limits.
#' @export
#' @examples
#' conc <- c(0.0625, 0.125, 0.25, 0.5, 1)
#' numtested <- rep(8, 5)
#' nalive <- c(1, 4, 4, 7, 8)
#' mydat <- dataprep(dose=conc, ntot=numtested, nfx=nalive)
#' myfit <- fitprobit(mydat)
#' coefprobit(myfit)

coefprobit <- function(pfit) {
	if (pfit$converged) {
		Pint <- coef(summary(pfit))[1, 1] +
      c(0, -1.96, 1.96)*coef(summary(pfit))[1, 2]
		Pslope <- coef(summary(pfit))[2, 1] +
      c(0, -1.96, 1.96)*coef(summary(pfit))[2, 2]
		} else {
		Pint <- rep(NA, 3)
		Pslope <- rep(NA, 3)
		}
	names(Pint) <- c("int", "ilower", "iupper")
	names(Pslope) <- c("slope", "slower", "supper")
	c(Pint, Pslope)
	}
