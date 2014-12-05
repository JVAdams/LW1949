LW1949 Vignette
===================

The following is an example using the 
functions in the **LW1949** package.  See [README.md](https://github.com/JVAdams/LW1949/blob/master/README.md) 
for instructions on installing the package.

Start by loading the package.

	library(LW1949)

Create a data frame with the results of a dose-effect experiment:

	conc <- c(0.0625, 0.125, 0.25, 0.5, 1, 2, 3)
	numtested <- rep(8, 7)
	numalive <- c(1, 4, 4, 7, 8, 8, 8)
	mydat <- dataprep(dose=conc, ntot=numtested, nfx=numalive)
	mydat

The resulting data frame will contain 
the record number (`rec`),
proportional effects (`pfx`), 
log transformed dose (`log10dose`),
probit transformed effects (`bitpfx`), 
an effects category (`fxcateg`) identifying none (0), partial (50), and complete (100) effects, and 
a column (`LWkeep`) to identify observations to keep when applying Litchfield and Wilcoxon's (1949) method:

		dose ntot nfx rec   pfx log10dose    bitpfx fxcateg LWkeep
	1 0.0625    8   1   1 0.125  -1.20412 -1.150349      50   TRUE
	2 0.1250    8   4   2 0.500  -0.90309  0.000000      50   TRUE
	3 0.2500    8   4   3 0.500  -0.60206  0.000000      50   TRUE
	4 0.5000    8   7   4 0.875  -0.30103  1.150349      50   TRUE
	5 1.0000    8   8   5 1.000   0.00000       Inf     100   TRUE

Fit a dose-effect relation following Litchfield and Wilcoxon's (1949) method:

	fLW <- fitLW(mydat)
	fLW

The results will include the results of a chi-square test (`chi`) comparing observed and expected effects and reporting the 
contribution to the chi-squared statistics, the estimated intercept and slope (`params`) on the log10-probit scale,
and additional estimates (`LWest`) calculated in the process of using Litchfield and Wilcoxon's (1949) method:

	$chi
	$chi$chi
	  chistat        df      pval 
	0.6365422 4.0000000 0.9589177 

	$chi$stepB
			   exp   expcorr      contrib
	[1,] 0.1713430 0.1713430 0.1002747397
	[2,] 0.3876399 0.3876399 0.2605466887
	[3,] 0.6472552 0.6472552 0.2680127749
	[4,] 0.8511356 0.8511356 0.0053529169
	[5,] 0.9558760 0.9854862 0.0017100210
	[6,] 0.9910604 0.9910604 0.0006451054


	$params
	(Intercept)   log10dose 
	   1.704712    2.203754 

	$LWest
		   ED50       lower       upper        ED16        ED84           S      lowerS      upperS      Nprime       fED50          fS 
	 0.16844208  0.09360509  0.30311101  0.05959286  0.47610960  2.82654788  1.57800874  5.06294592 24.00000000  1.79949695  1.79121180 

Use the fitted Litchfield and Wilcoxon model to estimate the effective doses for specified percent effects:

	pctalive <- c(25, 50, 99.9)
	predlinear(pctalive, fLW)

These estimates also include the 95% confidence limits for the effective doses:

		  pct         ED      lower      upper
	[1,] 25.0 0.08325045 0.04105587  0.1688099
	[2,] 50.0 0.16844208 0.09360509  0.3031110
	[3,] 99.9 4.25313344 0.63953931 28.2846474

Fit a dose-effect relation using a standard probit regression:

	fp <- fitprobit(mydat)

Use the fitted probit model to estimate the effective dose for a specified percent effect:

	cbind(pctalive, do.call(rbind, lapply(pctalive, predprobit, fp)))

These estimates also include the 95% confidence limits for the effective doses:

		 pctalive         ED      lower      upper
	[1,]     25.0 0.09169105 0.05095361  0.1649981
	[2,]     50.0 0.17058683 0.10976073  0.2651209
	[3,]     99.9 2.93241836 0.73983726 11.6229310

Plot the results from the two methods:

	plotDE(mydat)
	abline(fp$coef, lty=2)
	abline(fLW$params)
	legend("topleft", c("LW", "Probit"), lty=c(1, 2), bg="white")

![Dose-effect relation](https://github.com/JVAdams/LW1949/blob/master/Capture.PNG)
