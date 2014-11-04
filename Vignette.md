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

The resulting data frame will contain proportional effects (`pfx`), log transformed dose (`log10dose`),
probit transformed effects (`bitpfx`), an effects category (`fxcateg`) identifying none (0), partial (50), and complete affects,
and a column (`LWkeep`) to identify observations to keep when applying Litchfield and Wilcoxon's (1949) method:

		dose ntot nfx   pfx log10dose    bitpfx fxcateg LWkeep
	1 0.0625    8   1 0.125  -1.20412 -1.150349      50   TRUE
	2 0.1250    8   4 0.500  -0.90309  0.000000      50   TRUE
	3 0.2500    8   4 0.500  -0.60206  0.000000      50   TRUE
	4 0.5000    8   7 0.875  -0.30103  1.150349      50   TRUE
	5 1.0000    8   8 1.000   0.00000       Inf     100   TRUE

Fit a dose-effect relation following Litchfield and Wilcoxon's (1949) method:

	fLW <- fitLW(mydat)
	fLW

The results will include the results of a chi-square test (`chi`) comparing observed and expected effects and reporting the 
contribution to the chi-squared statistics, the estimated intercept and slope (`params`) on the log10-probit scale,
and additional estimates (`LWest`) calculated in the process of using Litchfield and Wilcoxon's (1949) method:

	$chi
	$chi$chi
	  chistat        df      pval 
	0.6358878 3.0000000 0.8881698 

	$chi$stepB
			   exp   expcorr     contrib
	[1,] 0.1716222 0.1716222 0.101321608
	[2,] 0.3875576 0.3875576 0.260984030
	[3,] 0.6466877 0.6466877 0.266184610
	[4,] 0.8504760 0.8504760 0.005657322
	[5,] 0.9554873 0.9853597 0.001740192


	$params
	(Intercept)   log10dose 
	   1.700561    2.199395 

	$LWest
		   ED50       lower       upper        ED16        ED84           S      Nprime         f50 
	 0.16857962  0.09357250  0.30371195  0.05951882  0.47748067  2.83237485 24.00000000  1.80159356 

Fit a dose-effect relation using a standard probit regression:

	fp <- fitprobit(mydat)

Use the fitted probit model to estimate the effective dose for a specified percent effect:

	pctalive <- c(16, 50, 84)
	cbind(pctalive, do.call(rbind, lapply(pctalive, predprobit, fp)))

These estimates also include the 95% confidence limits for the effective doses:

		 pctalive         ED      lower     upper
	[1,]       16 0.06705061 0.03224543 0.1394239
	[2,]       50 0.17111040 0.10918246 0.2681637
	[3,]       84 0.43666669 0.23649766 0.8062566

Plot the results from the two methods:

	plotDE(mydat)
	abline(fp$coef, lty=2)
	abline(fLW$params)

![Dose-effect relation](https://github.com/JVAdams/LW1949/blob/master/Capture.PNG)
