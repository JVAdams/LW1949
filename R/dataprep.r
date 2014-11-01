#' Prepare Data
#'
#' Prepare dose-effect data for evaluation.
#' @param dose 	A numeric vector of chemical concentrations.
#' @param ntot 	A numeric vector of the number of individuals that were tested at each dose.
#' @param ndead A numeric vector of the number of individuals that died at each dose.
#' @return 		a data frame with eight columns, seven numeric vectors and one logical vector:
#'	dose - chemical concentrations.
#'	ntot - the number of individuals that were tested at each dose.
#'	ndead - the number of individuals that died at each dose.
#'	pdead - the proportion of individuals that died at each dose.
#'	logdose - log transformed dose, \code{log10(dose)}.
#'	bitpdead - probit transformed proportional mortality, \code{\link{probit}(pdead)}.
#'	mcateg - mortality category: 0 for none dead, 100 for all dead, and 50 for any proportional mortality.
#'	LWkeep - logical vector identifying records to keep for Litchfield and Wilcoxon (1949, step A1) method.
#' @export
#' @references Litchfield, JT Jr. and F Wilcoxon.  1949.
#'	\href{http://jpet.aspetjournals.org/content/96/2/99.abstract}{A simplified method of evaluating dose-effect experiments}. 
#'	Journal of Pharmacology and Experimental Therapeutics 99(2):99-113.
#' @examples 
#' conc <- c(0.0625, 0.125, 0.25, 0.5, 1)
#' numtested <- rep(8, 5)
#' nmort <- c(1, 4, 4, 7, 8)
#' dataprep(dose=conc, ntot=numtested, ndead=nmort)

dataprep <- function(dose, ntot, ndead) {
	# create a data frame
	df <- data.frame(dose=dose, ntot=ntot, ndead=ndead)
	# calculate proportion dead
	df$pdead <- df$ndead/df$ntot
	# order data frame
	df <- df[order(df$dose, df$pdead), ]
	# transform variables
	df$logdose <- log10(df$dose)
	df$bitpdead <- probit(df$pdead)
	# define three mortality categories, 0 for none dead, 100 for all dead, and 50 for any proportional mortality
	df$mcateg <- mcat(df)
	# define records to keep for Litchfield Wilcoxon method
	# get rid of consecutive 0% and 100%s
	# A 1. Don't list > 2 consecutive 100% effects at the upper end or > 2 consecutive 0% effects at the lower end.
	df$LWkeep <- keeponly(df$pdead)
	# get rid of any zero dosages (controls)
	df$LWkeep[df$dose == 0] <- FALSE
	df
	}
