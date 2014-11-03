#' Fit a Probit Regression to Dose-Effect Data
#'
#' Fit a probit regression to dose-effect data, using the log10 of the dose, the binomial family, and the probit link.
#' @param DEdata	A data frame of dose-effect data (typically, the output from \code{\link{dataprep}}
#'	containing eight variables: dose, ntot, nfx, pfx, logdose, bitpfx, fxcateg, and LWkeep.
#' @return 			A an object of class \code{\link{glm}}.
#' @export
#' @examples 
#' conc <- c(0.0625, 0.125, 0.25, 0.5, 1)
#' numtested <- rep(8, 5)
#' nalive <- c(1, 4, 4, 7, 8)
#' mydat <- dataprep(dose=conc, ntot=numtested, nfx=nalive)
#' fitprobit(mydat)

fitprobit <- function(DEdata) {
	sel <- DEdata$dose > 0
	glm(cbind(nfx, ntot-nfx) ~ log10(dose), family=binomial(link=probit), data=DEdata[sel, ])
	}
