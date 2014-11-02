#' Determine if Lethal Concentrations are Estimable
#'
#' Determine if lethal concentrations are estimable based on available dose-effect data.
#' @param DEdata 	A data frame of dose-effect data (typically, the output from \code{\link{dataprep}}
#'	containing at least two variables: dose, a numeric vector of chemical concentrations, and pfx, 
#'	a numeric vector of proportional effects at each dose.
#' @return 			A logical scalar indicating if lethal concentrations are estimable.  If FALSE, a warning is generated.
#' @export
#' @details			Lethal concentrations are defined to be estimable (with error) if and only if there are at least three test records and
#'	there is some (non-zero) variability in both the doses and the proportional effects.
#' @examples 
#' conc <- c(0.0625, 0.125, 0.25, 0.5, 1)
#' numtested <- rep(8, 5)
#' nalive <- c(1, 4, 4, 7, 8)
#' mydat <- dataprep(dose=conc, ntot=numtested, nfx=nalive)
#' estimable(mydat)
#' nalive2 <- rep(4, 5)
#' mydat2 <- dataprep(dose=conc, ntot=numtested, nfx=nalive2)
#' estimable(mydat2)

estimable <- function(DEdata) {
	nonmiss <- !is.na(DEdata$dose) & !is.na(DEdata$pfx)
	if(sum(nonmiss) < 2.5) {
		out <- FALSE
		} else {
		out <- !(var(DEdata$dose[nonmiss]) < 0.00000001 | var(DEdata$pfx[nonmiss]) < 0.00000001)
		}
	if(!out) warning("Lethal concentrations not estimable.")
	out
	}
