#' Plot Dose-Effect Experiments
#'
#' Plot the results of dose-effect experiments.
#' @param DEdata 	A data frame of dose-effect data (typically, the output from \code{\link{dataprep}})
#'	containing at least five variables: dose, pfx, log10dose, bitpfx, fxcateg.
#' @param xlab 		A character scalar, the title for the dose (x) axis, default "Dose".
#' @param ylab 		A character scalar, the title for the affects (y) axis, default "Affected  (\%)".
#' @param ylim		A numeric vector of length two giving the y coordinate range for affects (\%), default c(0.1, 99.9).  
#'	Observed effects beyond this range will be plotted at the limits of this range using an open symbol.
#' @param ...		Additional arguments to \code{\link{plot}}.
#' @export
#' @examples 
#' dose <- c(0.0625, 0.125, 0.25, 0.5, 1)
#' ntested <- rep(8, 5)
#' nalive <- c(1, 4, 4, 7, 8)
#' mydat <- dataprep(dose=dose, ntot=ntested, nfx=nalive)
#' # just plot the raw data
#' plotDE(mydat)
#' # plot the raw data and some fitted lines
#' fLW <- fitLW(mydat)
#' fp <- fitprobit(mydat)
#' plotDE(mydat)
#' abline(fp$coef, lty=2)
#' abline(fLW$params)
#' legend("topleft", c("Litchfield-Wilcoxon", "Probit"), lty=c(1, 2), bg="white")

plotDE <- function(DEdata, xlab="Dose", ylab="Affected  (%)", ylim=c(0.1, 99.9), ...) {
	xtix <- prettylog(DEdata$dose, 1:9, 5)
	# xtix <- axTicks(side=2, axp=c(range(pretty(DEdata$dose)), -4), log=TRUE, nintLog=Inf)

	plot(DEdata$log10dose, DEdata$bitpfx, type="n", xlim=range(  log10( c(DEdata$dose, xtix[xtix>0]) ), na.rm=TRUE), ylim=probit(ylim/100), 
		axes=F, xlab=xlab, ylab=ylab, ...)

	# background grid and axes
	abline(v=log10(xtix), lwd=2, col="lightgray")
	axis(1, at=log10(xtix), labels=xtix)

	ytix1 <- c(seq(0.1, 0.9, 0.1), seq(1, 9, 1), seq(10, 90, 10), seq(91, 99, 1), seq(99.1, 99.9, 0.1))
	ytix2 <- c(0.1, 1, 10, 50, 90, 99, 99.9)
	abline(h=probit(ytix1/100), col="lightgray")
	abline(h=probit(ytix2/100), lwd=2, col="lightgray")
	axis(2, at=probit(ytix2/100), labels=ytix2, las=1)
	box()

	# observed points
	points(DEdata$log10dose, probit(constrain(DEdata$pfx, ylim/100)), pch=16, cex=1.5)
	points(DEdata$log10dose[DEdata$fxcateg!=50], probit(constrain(DEdata$pfx[DEdata$fxcateg!=50], ylim/100)), pch=16, col="white")
	}
