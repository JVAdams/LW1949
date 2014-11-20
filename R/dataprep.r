#' Prepare Data
#'
#' Prepare dose-effect data for evaluation.
#' @param dose 	A numeric vector of chemical concentrations.
#' @param ntot 	A numeric vector of the number of individuals that were tested at each dose.
#' @param nfx	A numeric vector of the number of individuals that were affected at each dose.
#' @return 		A data frame with eight columns (ordered by dose and proportion affected), seven numeric vectors and one logical vector:
#'	dose - chemical concentrations.
#'	ntot - the number of individuals that were tested at each dose.
#'	nfx - the number of individuals that were affected at each dose.
#'	rec - the record number corresponding to the input vectors \code{dose}, \code{ntot}, \code{nfx}.
#'	pfx - the proportion of individuals that were affected at each dose.
#'	log10dose - log transformed dose, \code{log10(dose)}.
#'	bitpfx - probit transformed proportional affected, \code{\link{probit}(pfx)}.
#'	fxcateg - effects category: 0 for none affected, 100 for all affected, and 50 for other proportions affected.
#'	LWkeep - logical vector identifying records to keep for Litchfield and Wilcoxon (1949, step A1) method.
#' @export
#' @references Litchfield, JT Jr. and F Wilcoxon.  1949.
#' A simplified method of evaluating dose-effect experiments. 
#' Journal of Pharmacology and Experimental Therapeutics 99(2):99-113.
#' \href{http://jpet.aspetjournals.org/content/96/2/99.abstract}{[link]}. 
#' @examples 
#' conc <- c(0.0625, 0.125, 0.25, 0.5, 1)
#' numtested <- rep(8, 5)
#' nalive <- c(1, 4, 4, 7, 8)
#' dataprep(dose=conc, ntot=numtested, nfx=nalive)

dataprep <- function(dose, ntot, nfx) {
	# create a data frame
	df <- data.frame(dose=dose, ntot=ntot, nfx=nfx)
	# assign row number
	df$rec <- 1:dim(df)[1]
	# calculate proportion affected
	df$pfx <- df$nfx/df$ntot
	# order data frame
	df <- df[order(df$dose, df$pfx), ]
	# transform variables
	df$log10dose <- log10(df$dose)
	df$bitpfx <- probit(df$pfx)
	# define three effect categories, 0 for none affected, 100 for all affected, and 50 for other proportions affected
	df$fxcateg <- fxcat(df)
	# define records to keep for Litchfield Wilcoxon method
	# get rid of consecutive 0% and 100%s
	# A 1. Don't list > 2 consecutive 100% effects at the upper end or > 2 consecutive 0% effects at the lower end.
	df$LWkeep <- keeponly(df$pfx)
	# get rid of any missing doses or effects
	df$LWkeep[is.na(df$dose) | is.na(df$pfx)] <- FALSE
	# get rid of any zero doses (controls)
	df$LWkeep[df$dose == 0] <- FALSE
	df
	}
