LW1949 Vignette
===================

The following is an example using the 
functions in the **LW1949** package.  See [README.md](https://github.com/JVAdams/LW1949/blob/master/README.md) 
for instructions on installing the package.

Start by loading the package.

	library(LW1949)

Create a data frame with the results of a dose-effect experiment:

	conc <- c(0.0625, 0.125, 0.25, 0.5, 1)
	numtested <- rep(8, 5)
	numalive <- c(1, 4, 4, 7, 8)
	mydat <- dataprep(dose=conc, ntot=numtested, nfx=numalive)
	mydat

Fit a dose-effect relation following Litchfield and Wilcoxon's (1949) method:

	fLW <- fitLW(mydat)

Fit a dose-effect relation using a standard probit regression:

	fp <- fitprobit(mydat)
	fp

Look at the results from the two methods:

	fLW
	pctalive <- c(16, 50, 84)
	cbind(pctalive, do.call(rbind, lapply(pctalive, predprobit, fp)))

Plot the results from the two methods:

	plotDE(mydat)
	abline(fp$coef, lty=2)
	abline(fLW$params)
