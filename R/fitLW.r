#' Apply Litchfield and Wilcoxon Evaluation of Dose-Effect Experiments
#'
#' Automatically apply Litchfield and Wilcoxon's (1949) evaluation of dose-effect experiments.
#' @param DEdata 	A data frame of dose-effect data (typically, the output from \code{\link{dataprep}}
#'	containing at eight variables: dose, ntot, nfx, pfx, logdose, bitpfx, fxcateg, and LWkeep.
#' @return 		A list of length three:
#'  chi = the chi-squared statistic with associated P value and degrees of freedom,
#'  params = the estimated intercept and slope of the dose-response curve on the log10 probit scale,
#'  LWest = the Litchfield Wilcoxon estimates of ED50 with 95% confidence intervals and other metrics used in their step-by-step approach
#'  (ED16, ED84, S, and slope).
#' @export
#' @examples 
#' dose <- c(0.0625, 0.125, 0.25, 0.5, 1)
#' numtested <- rep(8, 5)
#' numalive <- c(1, 4, 4, 7, 8)
#' mydat <- dataprep(dose=dose, ntot=numtested, nfx=numalive)
#' mydat
#' fitLW(mydat)

fitLW <- function(DEdata) {
	if(!estimable(DEdata)) {
		out <- list(chi=rep(NA, 3), params=rep(NA, 2), LWest=rep(NA, 8))
		} else {
		dfsub <- DEdata[DEdata$LWkeep, ]

		# fit a smooth GAM function to expected and corrected values in Table 1 of Litchfield and Wilcoxon (1949)
		gamfit <- gamtable1()

		# calculate starting values for the int and slope using simple linear regression
		pms <- sum(dfsub$fxcateg==50)
		sv <- c(NA, NA)
		svchi <- NA
		# fit line to partial effects alone
		if(pms > 1) {
			dfpart <- dfsub[dfsub$fxcateg==50, ]
			sv <- fitlinear(dfpart, gamfit)
			svchi <- assessfit(sv, dfpart, gamfit)
			}
		# fit line to partial effects with last 0% and first 100%
		if(pms==1 | is.na(svchi)) {
			dfpart <- rbind(dfsub[dfsub$fxcateg==0, ][sum(dfsub$fxcateg==0), ], dfsub[dfsub$fxcateg==50, ], dfsub[dfsub$fxcateg==100, ][1, ])
			sv <- fitlinear(dfpart, gamfit)
			svchi <- assessfit(sv, dfpart, gamfit)
			}
		# fit line to all the data
		if(pms < 1 | is.na(svchi)) {
			sv <- fitlinear(dfpart, gamfit)
			svchi <- assessfit(sv, dfpart, gamfit)
			}
		# fit line to first 0% and last 100% alone
		if(is.na(svchi)) {
			dfpart <- rbind(dfsub[dfsub$fxcateg==0, ][1, ], dfsub[dfsub$fxcateg==100, ][sum(dfsub$fxcateg==100), ])
			sv <- fitlinear(dfpart, gamfit)
			svchi <- assessfit(sv, dfpart, gamfit)
			}

		### B1, B2, and C are all inside the function assessfit()
		# B1. If the expected value for any 0% or 100% dose is < 0.01% or > 99.99%, delete record
		# B2. Using the expected effect, record a corrected value for each 0 and 100% effect

		### C. The chi squared test
		# find the parameters that yield the best fit in the log10dose * probit space, by minimizing the chi squared
		estparams <- optim(par=sv, fn=assessfit, DEdata=dfsub, fit=gamfit)$par
		chi <- assessfit(estparams, DEdata=dfsub, fit=gamfit, simple=FALSE)
		if(!is.na(chi$chi["chistat"]) & chi$chi["pval"] < 0.05) warning("Chi squared test indicates poor fit.")

		### D1. Read from the line on the graph the dose for 16, 50, and 84% effects
		ED16 <- predlinear(16, b0=estparams[1], b1=estparams[2])
		ED50 <- predlinear(50, b0=estparams[1], b1=estparams[2])
		ED84 <- predlinear(84, b0=estparams[1], b1=estparams[2])
		# D2. Calculate the Litchfield and Wilcoxon (1949) slope function, S
		S <- (ED84/ED50 + ED50/ED16) / 2
		# D3. Obtain Nprime, the total number of animals tested at those doses with expected effects between 16 and 84%.
		Nprime <- sum(dfsub$ntot[dfsub$dose > ED16 & dfsub$dose < ED84])
		# D4. Calculate S to the exponent for the ED50
		f50 <- S^(2.77/sqrt(Nprime))
		# D5. Calculate the 95% confidence limits of the ED50 as
		upper50 <- ED50 * f50
		lower50 <- ED50 / f50

		### I'm skipping the rest of the steps
		# E. Calculate the confidence limits of S (requires mysterious Nomograph No. 3)
		# F. Factors for significantly heterogeneous data
		# G. Test for parallelism of two lines and estimate of relative potency

		out <- list(chi=chi, params=c(int=estparams[1], slope=estparams[2]), 
			LWest=c(ED50=ED50, lower=lower50, upper=upper50, ED16=ED16, ED84=ED84, S=S, Nprime=Nprime, f50=f50))
		}
	out
	}
