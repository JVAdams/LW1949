#' Apply Litchfield and Wilcoxon Evaluation of Dose-Effect Experiments
#'
#' Automatically apply Litchfield and Wilcoxon's (1949) evaluation of dose-effect experiments.
#' @param dose 	A numeric vector of chemical concentrations.
#' @param ntot 	A numeric vector of the number of individuals that were tested at each dose.
#' @param ndead A numeric vector of the number of individuals that died at each dose.
#' @return 		A list of length three:
#'  chi = the chi-squared statistic with associated P value and degrees of freedom,
#'  params = the estimated LC50 and LC99.9 on the log10 scale used to describe the dose-response curve,
#'  LWest = the Litchfield Wilcoxon estimates of LC50 with 95% confidence intervals and other metrics used in their step-by-step approach
#'  (LC16, LC84, S, and slope).
#' @export
#' @examples 
#' conc <- c(0.0625, 0.125, 0.25, 0.5, 1)
#' numtested <- rep(8, 5)
#' nmort <- c(1, 4, 4, 7, 8)
#' LWauto(dose=conc, ntot=numtested, ndead=nmort)

LWauto <- function(dose, ntot, ndead) {

	### A. The data and graph.
	# A 1. Don't list > 2 consecutive 100% effects at the upper end or > 2 consecutive 0% effects at the lower end.
	df <- data.frame(dose=dose, ndead=ndead, ntot=ntot)
	df$pdead <- df$ndead/df$ntot
	df <- df[order(df$dose, df$pdead), ]
	# get rid of any zero dosages (controls)
	df <- df[df$dose > 0, ]
	# get rid of consecutives ...
	dfsub <- df[keeponly(df$pdead), ]

	# define three mortality categories, 0 for no dead, 100 for all dead, and 50 from any proportional mortality
	dfsub$mcat <- mcat(dfsub)

	# if we have < 3 observations or we only have all dead or all survive, we can't estimate LCs
	if((var(dfsub$mcat) < 0.00000001 & dfsub$mcat[1] != 50) | dim(dfsub)[1] < 3) {
		warning("Lethal concentrations cannot be calculated if fewer than three doses were tested or if mortality was 0% or 100% at all doses.")
		out <- list(chi=rep(NA, 3), params=rep(NA, 2), LWest=rep(NA, 8))
		} else {

		# A 2. Plot doses against % effect on logarithmic-probability paper
		dfsub$ldose <- log10(dfsub$dose)
		dfsub$pbpdead <- probit(dfsub$pdead)
		yr <- probit(c(0.001, 0.999))
		dfsub$pbpdead[dfsub$pdead==0] <- yr[1]
		dfsub$pbpdead[dfsub$pdead==1] <- yr[2]

		# fit a smooth GAM function to expected and corrected values in Table 1 of Litchfield and Wilcoxon (1949)
		gamfit <- gamtable1()

		# calculate starting values for the LC50 and LC99.9 using simple linear regression
		pms <- sum(dfsub$mcat==50)
		svp <- list(sv=c(NA, NA), p=NA)
		# fit line to partial mortalities alone
		if(pms > 1) {
			dfpart <- dfsub[dfsub$mcat==50, ]
			svp <- startvals(dfpart, gamfit)
			}
		# fit line to partial mortalities with last 0% and first 100%
		if(pms==1 | is.na(svp$p)) {
			dfpart <- rbind(dfsub[dfsub$mcat==0, ][sum(dfsub$mcat==0), ], dfsub[dfsub$mcat==50, ], dfsub[dfsub$mcat==100, ][1, ])
			svp <- startvals(dfpart, gamfit)
			}
		# fit line to all the data
		if(pms < 1 | is.na(svp$p)) {
			svp <- startvals(dfsub, gamfit)
			}
		# fit line to first 0% and last 100% alone
		if(is.na(svp$p)) {
			dfpart <- rbind(dfsub[dfsub$mcat==0, ][1, ], dfsub[dfsub$mcat==100, ][sum(dfsub$mcat==100), ])
			svp <- startvals(dfpart)
			}

		# B. 1., B. 2., and C. are all inside the function assessfit()
		# B. 1. If the expected value for any 0% or 100% dose is < 0.01% or > 99.99%, delete record
		# B. 2. Using the expected effect, record a corrected value for each 0 and 100% effect
		# C. The chi squared test
		# find the LC50 and LC99.9 (on the log10 scale) that give the lowest p value for the chi-square
		# find the parameters that yield the best fit in the log10dose * probit space, by maximizing the P value of the chi squared
		estLCs <- optim(par=svp$start, fn=assessfit, dat=dfsub, fit=gamfit, control=list(fnscale=-1))$par
		chi <- assessfit(estLCs, dat=dfsub, fit=gamfit, simple=FALSE)
		if(!is.na(chi["pval"]) & chi["pval"] < 0.05) warning("Chi squared test indicates poor fit.")

		# L-W
		slope <- as.numeric(probit(0.999) / (estLCs[2] - estLCs[1]))
		int <- -slope*estLCs[1]

		# D. 1. Read from the line on the graph the dose for 16, 50, and 84% effects
		LC16 <- LC(16, b0=int, b1=slope)
		LC50 <- LC(50, b0=int, b1=slope) # same as 10^estLCs[1]
		LC84 <- LC(84, b0=int, b1=slope)
		# D. 2. Calculate the slope function, S
		S <- (LC84/LC50 + LC50/LC16) / 2
		# D. 3. Obtain Nprime, the total number of animals tested at those doses with expected effects
		#		between 16 and 84%.
		Nprime <- sum(dfsub$ntot[dfsub$dose > LC16 & dfsub$dose < LC84])
		# D. 4. Calculate S to the exponent for the LC50
		f50 <- S^(2.77/sqrt(Nprime))
		# D. 5. Calculate the 95% confidence limits of the LC50 as
		upper50 <- LC50 * f50
		lower50 <- LC50 / f50
		### I'm skipping the rest of the steps
		# E. Calculate the confidence limits of S (requires mysterious Nomograph No. 3)
		# F. Factors for significantly heterogeneous data
		# G. Test for parallelism of two lines and estimate of relative potency

		out <- list(chi=chi, params=c(int=int, slope=slope), 
			LWest=c(LC50=LC50, lower=lower50, upper=upper50, LC16=LC16, LC84=LC84, S=S, Nprime=Nprime, f50=f50))
		}
	out
	}
