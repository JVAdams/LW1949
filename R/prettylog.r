#' Pretty Breakpoints on Log Scale
#'
#' Compute a sequence of "round" values which cover the range of \code{x} on the log scale. 
#' @param x		A numeric vector.
#' @param lead	An integer vector giving the desired lead digits of pretty values on the log scale, default c(1, 5).
#' @param extra	An integer scalar giving the desired number of additional non-log scale values to include, default 5.
#' @return		A numeric vector of pretty values covering the range of \code{x} on the log scale.
#' @export
#' @examples 
#' vals <- rlnorm(100, 6)
#' summary(vals)
#' prettylog(vals, 1, 0)
#' prettylog(vals, 1)
#' prettylog(vals, c(1, 2, 5))

prettylog <- function(x, lead=c(1, 5), extra=5) {
	urd <- function(d, x) {
		lxd <- log10(x/d)
		rlxd <- unique(c(floor(lxd), ceiling(lxd)))
		d*10^rlxd
		}
	out <- sort(unlist(lapply(lead, urd, x)))
	if(extra>0) {
		out <- sort(unique(c(out, pretty(x, n=extra))))
		}
	out
	}
