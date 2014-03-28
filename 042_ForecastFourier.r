##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Compute a forecast based on a set of sine/cosine waves and
##     a series of date flags.
##  2. The fourier order is defined by the prior order search script
##     (but there is a floor b/c os some issues)
##  3. The dates are based on a visual scan of some of the fit
##     results ... and so need to be optimized
##------------------------------------------------------------------
options(warn=1)

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(forecast)

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/walmart/data/current")
wd	<- getwd()

##------------------------------------------------------------------
## Load data
##------------------------------------------------------------------
load("005_walmartCombinedData_20140314.Rdata")                              ## raw data
load("./OrderSearch/040_FourierOrderSearch_Only40_All_20140326.Rdata")      ## optimal fourier coefficients

##------------------------------------------------------------------
## Source Utilities
##------------------------------------------------------------------
source("/Users/alexstephens/Development/kaggle/walmart/k_wmt/000_UtilityFunctions.r")

##------------------------------------------------------------------
## Constants
##------------------------------------------------------------------

## all data
uniqStores      <- uniq.list$uniqStores
uniqDept        <- uniq.list$uniqDept
uniqSd          <- uniq.list$uniqSd
numStores       <- uniq.list$numStores
numDept         <- uniq.list$numDept
numSd           <- uniq.list$numSd

## test data
uniqTestStores	<- uniq.list$uniqTestStores
uniqTestDept	<- uniq.list$uniqTestDept
uniqTestSd		<- uniq.list$uniqTestSd
numTestStores	<- uniq.list$numTestStores
numTestDept		<- uniq.list$numTestDept
numTestSd		<- uniq.list$numTestSd

## time
numWeek         <- 52
minTime         <- 5
maxTime         <- 186

## minimum requirements for a fit
minObs          <- 100

##------------------------------------------------------------------
## Main
##------------------------------------------------------------------

##------------------------------------------------------------------
## Process the fit file
##------------------------------------------------------------------
orderCoef.list  <- list()
fit.names       <- names(orders.list)

for (i in 1:length(fit.names)) {

    ## get the AIC data, but set a low-limit to the order == 5
    tmp.name    <- fit.names[i]
    tmp.coef    <- orders.list[[tmp.name]]$coef
    #tmp.res     <- orders.list[[tmp.name]]$res
    #tmp.res     <- tmp.res[ tmp.res[,1] >=5 , ]
    
    ## then pull off the order with the minimum in-sample AICc
    #tmp.k       <- tmp.res[ which(tmp.res[,2] == min(tmp.res[,2])) , 1][1]
    #tmp.k                           <- tmp.res[2]
    #orderCoef.list[[tmp.name]]$k    <- tmp.k
    orderCoef.list[[tmp.name]]$coef <- names(tmp.coef)[2:length(tmp.coef)]
    
}


##------------------------------------------------------------------
## store results in a list
##------------------------------------------------------------------
vanilla.list		<- list()

##------------------------------------------------------------------
## Loop over all of the test s/d combos and compute the forecast
##------------------------------------------------------------------
for (i in 1:numTestSd) {


    ## grab s/d parameters from the clean.list()
	tmp.sd		<- as.character(droplevels(uniqTestSd[i]))
	tmp.sdName	<- paste("SD", tmp.sd, sep="_")
	tmp.store	<- clean.list[[tmp.sdName]]$store
	tmp.dept	<- clean.list[[tmp.sdName]]$dept
	tmp.len		<- clean.list[[tmp.sdName]]$orig.len
    tmp.expl_fl	<- clean.list[[tmp.sdName]]$expl_fl
	
	## define dataframes
	tmp.dat		<- clean.list[[tmp.sdName]]$data	
	tmp.tr_fl	<- tmp.dat$tr_fl
	tmp.wgt		<- ifelse(tmp.dat$isholiday, 5, 1)
	tmp.hist	<- tmp.dat[ (tmp.tr_fl == 1) , ]			## historical data
	tmp.proj	<- tmp.dat[ (tmp.tr_fl == 0) , ]			## projection data

    ## grab fourier coefficients
    tmp.coef    <- orderCoef.list[[tmp.sdName]]$coef
    
	## number of original observations
	num.obs		<- sum(!is.na(tmp.hist$weekly_sales))

	## define a filename for the plot
	tmp.folder		<- paste(wd, "/ForecastVanillaFourier_0326/", substr(tmp.sd,4,5), "/", sep="")
	tmp.filename	<- paste(tmp.folder, tmp.sdName, ".ForecastFourierResults.pdf", sep="")
	dir.create(tmp.folder, showWarnings = FALSE)

	##------------------------------------------------------------------
	## basic stl projection
	##------------------------------------------------------------------
	if ( (tmp.store > 0) & (tmp.dept > 0) ) {
		if ((num.obs > minObs) & (tmp.sd != "43_28")) {
            if ( !is.null(orderCoef.list[[tmp.sdName]]) ) {

                ## report progress
                cat("Processing: SD = ", tmp.sdName, "\n")

                ## grab the data
                ws	<- tmp.dat$ws.min10[ (tmp.tr_fl == 1) ]     ## min10 because of BoxCox transform

                ## grab the fourier order based on the order search
                #k   <- orderCoef.list[[tmp.sdName]]$k
                #k   <- max(20, orderCoef.list[[tmp.sdName]]$k)      ## force minimum fit of order 20
                
                ##------------------------------------------------------------------
                ## Questionable whether or not the standard set of holiday flags
                ## is causing overshooting on some of the fits.  May need to remove
                ## c("td_m01","td_m00","xm_m01") for some of these.  Can experiment
                ## by setting standard holidays to NULL
                ##------------------------------------------------------------------

                ##------------------------------------------------------------------
                ## [~][d01] -
                ##------------------------------------------------------------------
                if (tmp.dept == 1) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","va_m00","ea_m01","ea_m00","ha_m01","ha_m00","xm_m02","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("sb_m00","va_m00","ea_m01","ea_m00","ha_m01","ha_m00","xm_m02","xm_m01")]
                    ## order constraints
                    ##k            <- max(6,orderCoef.list[[tmp.sdName]]$k)
                ##------------------------------------------------------------------
                ## [+][d02] - s22/s23/s29/s39
                ##------------------------------------------------------------------
                } else if (tmp.dept == 2) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                ##------------------------------------------------------------------
                ## [+][d03] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 3) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                ##------------------------------------------------------------------
                ## [+][d04] - similar response as d05
                ##------------------------------------------------------------------
                } else if (tmp.dept == 4) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("sb_m00","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                ##------------------------------------------------------------------
                ## [+][d05] - dates important
                ##------------------------------------------------------------------
                } else if (tmp.dept == 5) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("sb_m00","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                ##------------------------------------------------------------------
                ## [~][d06] - can revisit, since there are a low of downward trends
                ##------------------------------------------------------------------
                } else if (tmp.dept == 6) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("mo_m00","fj_m00","td_m00","xm_m02","xm_m01")] #,"td_m01"
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("mo_m00","fj_m00","td_m00","xm_m02","xm_m01")] #,"td_m01","td_m00","xm_m02","xm_m01"
                ##------------------------------------------------------------------
                ## [+][d07] - spike dom; md/fj/td/xm; [?] can add "td_m00","td_p01" for separation [?]
                ##------------------------------------------------------------------
                } else if (tmp.dept == 7) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00")]
                ##------------------------------------------------------------------
                ## [+][d08] - excess s02/s17/s23/s24/s25/s35/s39/s40/s41 (caps???)
                ##------------------------------------------------------------------
                } else if (tmp.dept == 8) {
                    tmp.hhol     <- NULL
                    tmp.phol     <- NULL
                ##------------------------------------------------------------------
                ## [+][d09] - s33(use historical)
                ##------------------------------------------------------------------
                } else if (tmp.dept == 9) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                ##------------------------------------------------------------------
                ## [+][d10] - holidays may not add value
                ##------------------------------------------------------------------
                } else if (tmp.dept == 10) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","td_m01","td_m00","xm_m01")]
                ##------------------------------------------------------------------
                ## [+][d11] - memorial/july fourth spikes; some temperature correlations ?
                ##------------------------------------------------------------------
                } else if (tmp.dept == 11) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("md_m00","fj_m01","fj_m00","td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("md_m00","fj_m01","fj_m00","td_m01","td_m00","xm_m01")]
                ##------------------------------------------------------------------
                ## [+][d12] - no real spikes
                ##------------------------------------------------------------------
                } else if (tmp.dept == 12) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                ##------------------------------------------------------------------
                ## [+][d13] - no real spikes
                ##------------------------------------------------------------------
                } else if (tmp.dept == 13) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                ##------------------------------------------------------------------
                ## [d14] - no real spikes
                ##------------------------------------------------------------------
                } else if (tmp.dept == 14) {
                    tmp.hhol     <- NULL
                    tmp.phol     <- NULL
                ##------------------------------------------------------------------
                ## [+][d16] - no real spikes
                ##------------------------------------------------------------------
                } else if (tmp.dept == 16) {
                    tmp.hhol     <- NULL
                    tmp.phol     <- NULL
                ##------------------------------------------------------------------
                ## [+][d17]
                ##------------------------------------------------------------------
                } else if (tmp.dept == 17) {
                    tmp.hhol     <- NULL
                    tmp.phol     <- NULL
                ##------------------------------------------------------------------
                ## [+][d18] - hyperseasonal
                ##------------------------------------------------------------------
                } else if (tmp.dept == 18) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m00")]
                ##------------------------------------------------------------------
                ## [+~][d20] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 20) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00")]
                ##------------------------------------------------------------------
                ## [+~][d21] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 21) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00")]
                ##------------------------------------------------------------------
                ## [+~][d22] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 22) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00")]
                ##------------------------------------------------------------------
                ## [+][d23] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 23) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00")]
                ##------------------------------------------------------------------
                ## [+~][d24] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 24) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    
                ##------------------------------------------------------------------
                ## [+~][d25] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 25) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00")]
                ##------------------------------------------------------------------
                ## [+~][d26] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 26) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00")]
                ##------------------------------------------------------------------
                ## [+~][d29] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 29) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00")]
                   
                ##------------------------------------------------------------------
                ## [+][d32] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 32) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00")]
                ##------------------------------------------------------------------
                ## [+~][d33] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 33) {
                    tmp.hhol     <- NULL #holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    tmp.phol     <- NULL #holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00")]
                ##------------------------------------------------------------------
                ## [+][d34] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 34) {
                    tmp.hhol     <- NULL #holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    tmp.phol     <- NULL #holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00")]
                } else if (tmp.dept == 38) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                } else if (tmp.dept == 40) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                ##------------------------------------------------------------------
                ## [+][d46] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 46) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                ##------------------------------------------------------------------
                ## [+][d49] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 49) {
                    tmp.hhol     <- NULL #holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- NULL #holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                ##------------------------------------------------------------------
                ## [~][d55] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 55) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ld_m01","td_m00","xm_m02","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ld_m01","td_m00","xm_m02","xm_m01")]
                ##------------------------------------------------------------------
                ## [~][d58] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 58) {
                    tmp.hhol     <- NULL #holiday.df[ (tmp.tr_fl == 1), c("ld_m01","td_m00","xm_m02","xm_m01")]
                    tmp.phol     <- NULL #holiday.df[ (tmp.tr_fl == 0), c("ld_m01","td_m00","xm_m02","xm_m01")]
                ##------------------------------------------------------------------
                ## [+][d65] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 65) {
                    tmp.hhol     <- NULL
                    tmp.phol     <- NULL
                } else if (tmp.dept == 72) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                ##------------------------------------------------------------------
                ## [?][d74] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 74) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("fj_m00","td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("fj_m00","td_m01","td_m00","xm_m01")]
                ##------------------------------------------------------------------
                ## [+][d79] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 79) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("xm_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("xm_m00")]
                ##------------------------------------------------------------------
                ## [?][d80] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 80) {
                    tmp.hhol     <- NULL #holiday.df[ (tmp.tr_fl == 1), c("xm_m00")]
                    tmp.phol     <- NULL #holiday.df[ (tmp.tr_fl == 0), c("xm_m00")]
                ##------------------------------------------------------------------
                ## [+][d81] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 81) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                ##------------------------------------------------------------------
                ## [+][d82] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 82) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m00")]
                ##------------------------------------------------------------------
                ## [+][d87] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 87) {
                    tmp.hhol     <- NULL #holiday.df[ (tmp.tr_fl == 1), c("xm_m00")]
                    tmp.phol     <- NULL #holiday.df[ (tmp.tr_fl == 0), c("xm_m00")]
                } else if (tmp.dept == 90) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","td_m01","td_m00","xm_m01")]
                } else if (tmp.dept == 91) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                } else if (tmp.dept == 92) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m02","xm_m01","xm_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m02","xm_m01","xm_m00")]
                } else if (tmp.dept == 93) {
                    #tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    #tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    tmp.hhol     <- NULL
                    tmp.phol     <- NULL
                ##------------------------------------------------------------------
                ## [d94] Add Mother's day flag - s07/s10/s12/s21/s22/
                ##------------------------------------------------------------------
                } else if (tmp.dept == 94) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("va_m00","mo_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("va_m00","mo_m00")]
                } else if (tmp.dept == 95) {
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("fj_m01","fj_m00","xm_m01","xm_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("fj_m01","fj_m00","xm_m01","xm_m00")]
                ##------------------------------------------------------------------
                ## [?][d96] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 96) {
                    tmp.hhol     <- NULL #holiday.df[ (tmp.tr_fl == 1), c("xm_m00")]
                    tmp.phol     <- NULL #holiday.df[ (tmp.tr_fl == 0), c("xm_m00")]
                ##------------------------------------------------------------------
                ## [+][d97] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 97) {
                    tmp.hhol     <- NULL #holiday.df[ (tmp.tr_fl == 1), c("xm_m00")]
                    tmp.phol     <- NULL #holiday.df[ (tmp.tr_fl == 0), c("xm_m00")]
                ##------------------------------------------------------------------
                ## All Others
                ##------------------------------------------------------------------
                } else {
                    tmp.hhol     <- NULL
                    tmp.phol     <- NULL
                }
               
                ##------------------------------------------------------------------
                ## compute a basic stl projection
                ##------------------------------------------------------------------
                tmp.fit	<- calcFourierFit(ws, coeffs=tmp.coef, regs.hist=tmp.hhol, regs.proj=tmp.phol, k=40, h=39)
                ##tmp.fit	<- calcFourierFit(ws, regs.hist=tmp.hhol, regs.proj=tmp.phol, k=k, h=39)
                
                ##------------------------------------------------------------------
                ## plot the results
                ##------------------------------------------------------------------
                pdf(tmp.filename)
                    plot(c(tmp.fit$x,tmp.fit$forecast), type="n", main=tmp.sdName, xlab="Time Index", ylab="Weekly Sales")  ## null plot
                    points(tmp.fit$x, type="b", pch=20, col="grey", lwd=2)
                    points(c(tmp.fit$fitted, tmp.fit$forecast), type="b", pch=1, col="red")
                dev.off()
			
                ## save the results
                vanilla.list[[tmp.sdName]] <- tmp.fit
			
            } else {
		
                ## will hit if num.obs <= 50
                vanilla.list[[tmp.sdName]] <- NULL
			
            }
        }
    }
}

##------------------------------------------------------------------
## Save image
##------------------------------------------------------------------
##save(vanilla.list, file="042_VanillaFourier_20140326.Rdata")





