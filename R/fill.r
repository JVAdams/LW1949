#' Fill in Missing Values
#'
#' Fill in missing values in a vector, using the last recorded value.  
#' @param x 	A vector, can be character, numeric, or logical.
#' @return 		A vector the same length as \code{x}, with all NAs or ""s replace by the last value for the vector.
#' Note that and missing values at the beginning of the vector will not be replaced.
#' @export
#' @details		Similar to \code{\link[zoo]{na.locf}} in the \code{zoo} package, but works for "" in character vectors as well.
#' @examples 
#' numvec <- c(NA, 1:5, NA, NA, NA, 10:12, NA)
#' fill(numvec)
#'
#' charvec <- c("", letters[1:5], "", "", "", letters[10:12], "")
#' fill(charvec)

fill <- function(x)
	{
	# fill in a vector of values
	# assign to every NA or "" the last value for the vector
    y <- x
    last <- x[1]
    if(is.character(x)){
        for(i in 2:length(x))
            if(x[i]=="" | is.na(x[i])) y[i] <- last else last <- x[i]
        } else {
        for(i in 2:length(x))
            if(is.na(x[i])) y[i] <- last else last <- x[i]
        }
    y
	}
