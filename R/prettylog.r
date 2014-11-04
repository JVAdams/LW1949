#' Pretty Breakpoints on Log Scale
#'
#' Compute a sequence of "round" values which cover the range of \code{x} on the log scale. 
#' @param x		A numeric vector.
#' @param lead	An integer vector giving the desired lead digitis of pretty values on the log scale, default c(1, 5).
#' @return		A numeric vector of pretty values covering the range of \code{x} on the log scale.
#' @export
#' @examples 
#' vals <- rlnorm(100, 6)
#' summary(vals)
#' prettylog(vals, 1)
#' prettylog(vals, c(1, 2, 5))

prettylog <- function(x, lead=c(1, 5)) {
	urd <- function(d, x) {
		lxd <- log10(x/d)
		rlxd <- unique(c(floor(lxd), ceiling(lxd)))
		d*10^rlxd
		}
	sort(unlist(lapply(lead, urd, x)))
	}
