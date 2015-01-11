#' Apply Litchfield and Wilcoxon Evaluation of Dose-Effect Experiments
#'
#' Automatically apply Litchfield and Wilcoxon's (1949) evaluation of dose-effect experiments.
#' @param DEdata 	A data frame of dose-effect data (typically, the output from \code{\link{dataprep}})
#'	containing at least eight variables: dose, ntot, nfx, pfx, log10dose, bitpfx, fxcateg, and LWkeep.
#' @return 		A list of length three:
#' \itemize{
#'   \item \code{chi} = the chi-squared statistic with associated P value and degrees of freedom,
#'   \item \code{params} = the estimated intercept and slope of the dose-response curve on the log10 probit scale,
#'   \item \code{LWest} = the Litchfield Wilcoxon estimates of ED50 with 95\% confidence intervals and other metrics used in their 
#'	step-by-step approach (ED16, ED84, S, N', and fED50).
#' }
#' @export
#' @references Litchfield, JT Jr. and F Wilcoxon.  1949.
#' A simplified method of evaluating dose-effect experiments. 
#' Journal of Pharmacology and Experimental Therapeutics 99(2):99-113.
#' \href{http://jpet.aspetjournals.org/content/96/2/99.abstract}{[link]}. 
#' @examples 
#' dose <- c(0.0625, 0.125, 0.25, 0.5, 1)
#' ntested <- rep(8, 5)
#' nalive <- c(1, 4, 4, 7, 8)
#' mydat <- dataprep(dose=dose, ntot=ntested, nfx=nalive)
#' mydat
#' fitLW(mydat)

fitLW <- function(DEdata) {
	if(!estimable(DEdata)) {
		out <- list(chi=rep(NA, 3), params=rep(NA, 2), LWest=rep(NA, 8))
		} else {
		dfsub <- DEdata[DEdata$LWkeep, ]
		df0 <- dfsub[dfsub$fxcateg==0, ]
		df100 <- dfsub[dfsub$fxcateg==100, ]

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
			dfpart <- rbind(df0[with(df0, which.max(conc)), ], dfsub[dfsub$fxcateg==50, ], df100[with(df100, which.min(conc)), ])
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
			dfpart <- rbind(df0[with(df0, which.min(conc)), ], dfsub[dfsub$fxcateg==50, ], df100[with(df100, which.max(conc)), ])
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
		if(is.na(chi$chi["pval"])) {
			warning("Chi squared test cannot be conducted when fewer than two expected values are between 0.01% and 99.99%.")
			} else {
			if(chi$chi["pval"] < 0.05) {
				warning("Chi squared test indicates poor fit.")
				}
			}

		### D1. Read from the line on the graph the dose for 16, 50, and 84% effects
		ED16 <- predlinear(16, estparams, simple=TRUE)
		ED50 <- predlinear(50, estparams, simple=TRUE)
		ED84 <- predlinear(84, estparams, simple=TRUE)
		# D2. Calculate the Litchfield and Wilcoxon (1949) slope function, S
		S <- (ED84/ED50 + ED50/ED16) / 2
		# D3. Obtain Nprime, the total number of animals tested at those doses with expected effects between 16 and 84%.
		Nprime <- sum(dfsub$ntot[dfsub$dose > ED16 & dfsub$dose < ED84])
		# D4. Calculate S to the exponent for the ED50
		fED50 <- S^(2.77/sqrt(Nprime))
		# D5. Calculate the 95% confidence limits of the ED50
		if(is.finite(fED50)) {
			upper50 <- ED50 * fED50
			lower50 <- ED50 / fED50
			} else {
			upper50 <- NA
			lower50 <- NA
			warning("Confidence bounds cannot be estimated when no expected values are between 16% and 84%.")
			}

		# E. Calculate the confidence limits of S (requires mysterious Nomograph No. 3)
		# E1. Calculate the dosage range as a ratio, R
		R <- max(dfsub$dose)/min(dfsub$dose)
		# E2. Calculate A from equation 6 in Appendix (Nomograph 3)
		A <- 10^( 1.1*(log10(S))^2 / log10(R) )
		# E3. Calculate K (the number of doses) and fS (Nomograph 2)
		K <- dim(dfsub)[1]
		fS <- A^( 10*(K-1) / (K * sqrt(Nprime)) )
		# E4. Calculate the 95% confidence limits of S
		upperS <- S * fS
		lowerS <- S / fS

		### I'm skipping the rest of the steps
		# F. Factors for significantly heterogeneous data
		# G. Test for parallelism of two lines and estimate of relative potency

		out <- list(chi=chi, params=estparams, 
			LWest=c(ED50=ED50, lower=lower50, upper=upper50, ED16=ED16, ED84=ED84, 
				S=S, lowerS=lowerS, upperS=upperS, Nprime=Nprime, fED50=fED50, fS=fS))
		}
	out
	}
