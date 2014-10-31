#' Define Mortality Category
#'
#' Define three mortality categories, 0 for nonoe dead, 100 for all dead, and 50 for anything in between, partial mortality.
#' @param dat	A data frame of raw toxicity data, including these three variables:
#'	dose (the concentration of the applied chemical), ntot (the number of individuals tested), and ndead (the number of dead individuals).
#' @return		A numeric vector the same length as \code{prob} with quantiles on the probit scale. 
#' @export
#' @examples
#' test <- data.frame(
#' 	dose=c(0.0625, 0.125, 0.25, 0.5), 
#' 	ntot=rep(8, 4), 
#' 	ndead = c(0, 4, 6, 8))
#' cbind(test, mcat(test))

mcat <- function(dat) {
	categ <- rep(50, dim(dat)[1])
	categ[dat$ndead==0] <- 0
	categ[dat$ntot==dat$ndead] <- 100
	categ
	}
