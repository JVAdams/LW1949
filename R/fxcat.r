#' Define Effect Category
#'
#' Define three effect categories, 0 for none affected, 100 for all affected, and 50 for other proportions affected.
#' @param dat	A data frame of raw toxicity data, including these three variables:
#'	dose (the concentration of the applied chemical), ntot (the number of individuals tested), and nfx (the number of affected individuals).
#' @return		A numeric vector the same length as \code{prob} with quantiles on the probit scale. 
#' @export
#' @examples
#' test <- data.frame(
#' 	dose=c(0.0625, 0.125, 0.25, 0.5), 
#' 	ntot=rep(8, 4), 
#' 	nfx = c(0, 4, 6, 8))
#' cbind(test, fxcat(test))

fxcat <- function(dat) {
	categ <- rep(50, dim(dat)[1])
	categ[dat$nfx==0] <- 0
	categ[dat$ntot==dat$nfx] <- 100
	categ
	}
