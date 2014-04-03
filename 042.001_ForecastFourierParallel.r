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
library(foreach)
library(doMC)
library(MASS)
library(Hmisc)

##------------------------------------------------------------------
## Register the clusters
##------------------------------------------------------------------
registerDoMC(4)

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
## Load s/d data
##------------------------------------------------------------------
load("005_walmartCombinedData_20140326.Rdata")


##------------------------------------------------------------------
## Load order search data
##------------------------------------------------------------------
#load("041.001_GlmVariableSearch_Min5_Max40_FactorsEcon_HolidaysAll_WeightEqual_20140329.Rdata")
#orders.list <- fourierRegressionGlm.list
load("./OrderSearch/040_FourierOrderSearch_20140326.Rdata")   ## fourier order guesses

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
minOrder        <- 5
maxOrder        <- 40

## minimum requirements for a fit
minObs          <- 100


##------------------------------------------------------------------
## Process the fit file
##------------------------------------------------------------------
order.list     <- fourierRegression.list
fit.names       <- names(order.list)

for (i in 1:length(fit.names)) {

    ## get the AIC data, but set a low-limit to the order == 15
    tmp.name                    <- fit.names[i]
    tmp.res                     <- order.list[[tmp.name]]$res
    tmp.k                       <- tmp.res[ (tmp.res[,1] >= 15) & (tmp.res[,1] <= 25), ]
    order.list[[tmp.name]]$k    <- tmp.k[which(tmp.k[,2] == min(tmp.k[,2])), 1][1]

}

##------------------------------------------------------------------
## use sink() to monitor progress
##------------------------------------------------------------------
#vanilla.list    <- list()
writeLines(c(""), "forecastFourierParallel.txt")
## report progress
sink("forecastFourierParallel.txt", append=TRUE)

##------------------------------------------------------------------
## Loop over all of the test s/d combos and compute the forecast
##------------------------------------------------------------------
vanilla.list <- list()
vanilla.list <- foreach(i=1:numTestSd) %dopar% {

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
    
	## number of original observations
	num.obs		<- sum(!is.na(tmp.hist$weekly_sales))

    ## add holiday flags
    holiday.df$sb_m01                           <- Lag(holiday.df$sb_m00,-1)
    holiday.df$sb_m01[is.na(holiday.df$sb_m01)] <- 0
    
	## define a filename for the plot
	tmp.folder		<- paste(wd, "/TEST/", substr(tmp.sd,4,5), "/", sep="")
	tmp.filename	<- paste(tmp.folder, tmp.sdName, ".ForecastFourierResults.pdf", sep="")
	dir.create(tmp.folder, showWarnings = FALSE)

	##------------------------------------------------------------------
	## basic stl projection
	##------------------------------------------------------------------
	if ( (tmp.store > 0) & (tmp.dept %in% c(6,27)) ) {
		if ((num.obs > minObs) & (tmp.sd != "43_28")) {
            if ( !is.null(order.list[[tmp.sdName]]) ) {

                cat("Processing: SD = ", tmp.sdName, "\n")

                ## grab the data
                ws	<- tmp.dat$ws.min10[ (tmp.tr_fl == 1) ]     ## min10 because of BoxCox transform

                ## grab the fourier order based on the order search
                ##k   <- order.list[[tmp.sdName]]$k
                k <- 15
                
                ##------------------------------------------------------------------
                ## Questionable whether or not the standard set of holiday flags
                ## is causing overshooting on some of the fits.  May need to remove
                ## c("td_m01","td_m00","xm_m01") for some of these.  Can experiment
                ## by setting standard holidays to NULL
                ##------------------------------------------------------------------

                ##------------------------------------------------------------------
                ## [***][d01] -
                ##------------------------------------------------------------------
                if (tmp.dept == 1) {
                    
                    load("040.003_ArimaOrderSearch_Dept01.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","va_m00","ea_m01","ea_m00","ha_m01","ha_m00","xm_m02","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("sb_m00","va_m00","ea_m01","ea_m00","ha_m01","ha_m00","xm_m02","xm_m01")]

                ##------------------------------------------------------------------
                ## [+][d02] - s22/s23/s29/s39
                ##------------------------------------------------------------------
                } else if (tmp.dept == 2) {
                    
                    load("040.003_ArimaOrderSearch_Dept02.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [+][d03] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 3) {
 
                    load("040.003_ArimaOrderSearch_Dept03.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [+][d04] - similar response as d05
                ##------------------------------------------------------------------
                } else if (tmp.dept == 4) {
                    
                    load("040.003_ArimaOrderSearch_Dept04.10.18.34.56.67.79.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("sb_m00","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [***][d05] - dates important
                ##------------------------------------------------------------------
                } else if (tmp.dept == 5) {
                    
                    load("040.003_ArimaOrderSearch_Dept05.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m01","sb_m00","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("sb_m01","sb_m00","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]

                ##------------------------------------------------------------------
                ## [~][d06] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 6) {
                    
                    load("040.003_ArimaOrderSearch_Dept06.27.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("mo_m00","fj_m00","td_m00","xm_m02","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("mo_m00","fj_m00","td_m00","xm_m02","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [***][d07] - spike dom; added "td_m00","td_p01" for separation
                ##------------------------------------------------------------------
                } else if (tmp.dept == 7) {
                    
                    load("040.003_ArimaOrderSearch_Dept07.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00","td_m00","td_p01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00","td_m00","td_p01")]
   
                ##------------------------------------------------------------------
                ## [+][d08] - excess s02/s17/s23/s24/s25/s35/s39/s40/s41 (caps???)
                ##------------------------------------------------------------------
                } else if (tmp.dept == 8) {
                    
                    load("040.003_ArimaOrderSearch_Dept08.87.93.94.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [+][d09] - s33(use historical)
                ##------------------------------------------------------------------
                } else if (tmp.dept == 9) {
                    
                    load("040.003_ArimaOrderSearch_Dept09.11.82.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [+][d10] - holidays may not add value
                ##------------------------------------------------------------------
                } else if (tmp.dept == 10) {
                    
                    load("040.003_ArimaOrderSearch_Dept04.10.18.34.56.67.79.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [+][d11] - memorial/july fourth spikes; some temperature correlations ?
                ##------------------------------------------------------------------
                } else if (tmp.dept == 11) {
                    
                    load("040.003_ArimaOrderSearch_Dept09.11.82.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("md_m00","fj_m01","fj_m00","td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("md_m00","fj_m01","fj_m00","td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
## [+][d12] - no real spikes
                ##------------------------------------------------------------------
                } else if (tmp.dept == 12) {
                    
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [***][d13] - no real spikes
                ##------------------------------------------------------------------
                } else if (tmp.dept == 13) {
                    
                    load("040.003_ArimaOrderSearch_Dept13.32.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [***][d14] - no real spikes
                ##------------------------------------------------------------------
                } else if (tmp.dept == 14) {
                    
                    load("040.003_ArimaOrderSearch_Dept14.46.55.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","va_m00","td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("sb_m00","va_m00","td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [***][d16] - no real spikes k=20???
                ##------------------------------------------------------------------
                } else if (tmp.dept == 16) {
                    
                    load("040.003_ArimaOrderSearch_Dept16.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00")]

                ##------------------------------------------------------------------
                ## [+][d17]
                ##------------------------------------------------------------------
                } else if (tmp.dept == 17) {
                    
                    load("040.003_ArimaOrderSearch_Dept17.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [+][d18] - hyperseasonal
                ##------------------------------------------------------------------
                } else if (tmp.dept == 18) {
                    
                    load("040.003_ArimaOrderSearch_Dept04.10.18.34.56.67.79.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","va_m00","ea_m00","ha_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("sb_m00","va_m00","ea_m00","ha_m00")]
        
## [!!!][d19] - marginal

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
                    
                    load("040.003_ArimaOrderSearch_Dept23.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    
                ##------------------------------------------------------------------
                ## [+~][d24] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 24) {
                    
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                    
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
                    
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [+~][d27] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 27) {
                    
                    load("040.003_ArimaOrderSearch_Dept06.27.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("sb_m00","td_m01","td_m00","xm_m01")]

##------------------------------------------------------------------
## [+~][d28] -
##------------------------------------------------------------------
                } else if (tmp.dept == 28) {
                    
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [+~][d29] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 29) {
                    
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00")]
                   
##------------------------------------------------------------------
## [+~][d30] -
##------------------------------------------------------------------
                } else if (tmp.dept == 30) {
                    
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]

##------------------------------------------------------------------
## [+~][d31] -
##------------------------------------------------------------------
                } else if (tmp.dept == 31) {
                    
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","va_m00","ea_m01","ea_m00","md_m00","ld_m02","ld_m01","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("sb_m00","va_m00","ea_m01","ea_m00","md_m00","ld_m02","ld_m01","xm_m01")]

                ##------------------------------------------------------------------
                ## [+][d32] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 32) {
                    
                    load("040.003_ArimaOrderSearch_Dept13.32.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    
##------------------------------------------------------------------
## [+~][d33] -
##------------------------------------------------------------------
                } else if (tmp.dept == 33) {
                    
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","ea_m00","md_m00","ld_m03","ld_m02","ld_m01","td_m01","td_m00","xm_m02","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("sb_m00","ea_m00","md_m00","ld_m03","ld_m02","ld_m01","td_m01","td_m00","xm_m02","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [+][d34] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 34) {
                    
                    load("040.003_ArimaOrderSearch_Dept04.10.18.34.56.67.79.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00")]
 
## [!!!][d35] - fit
## [!!!][d36] - fit
## [!!!][d37] - marginal
  
                ##------------------------------------------------------------------
                ## [***][d38] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 38) {
                    
                    load("040.003_ArimaOrderSearch_Dept38.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
## [!!!][d39] - skip
                ##------------------------------------------------------------------
                ## [***][d40] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 40) {
                    
                    load("040.003_ArimaOrderSearch_Dept40.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    
## [!!!][d41] - fit
## [!!!][d42] - fit
## [!!!][d43] - skip
## [!!!][d44] - fit
## [!!!][d45] - skip


                ##------------------------------------------------------------------
                ## [***][d46] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 46) {
                    
                    load("040.003_ArimaOrderSearch_Dept14.46.55.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
## [!!!][d47] - skip
## [!!!][d48] - fit
                ##------------------------------------------------------------------
                ## [+][d49] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 49) {
                    
                    tmp.hhol     <- NULL #holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- NULL #holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
## [!!!][d50] - fit
## [!!!][d51] - skip
## [!!!][d52] - needs a fit
## [!!!][d54] - marginal

                ##------------------------------------------------------------------
                ## [***][d55] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 55) {
                    
                    load("040.003_ArimaOrderSearch_Dept14.46.55.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m02","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m02","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [***][d56] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 56) {
                    
                    load("040.003_ArimaOrderSearch_Dept04.10.18.34.56.67.79.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m00","xm_m02","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m00","xm_m02","xm_m01")]
 
                ##------------------------------------------------------------------
                ## [~][d58] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 58) {
                    
                    tmp.hhol     <- NULL #holiday.df[ (tmp.tr_fl == 1), c("ld_m01","td_m00","xm_m02","xm_m01")]
                    tmp.phol     <- NULL #holiday.df[ (tmp.tr_fl == 0), c("ld_m01","td_m00","xm_m02","xm_m01")]
                    
## [!!!][d59] - very peculiar; 1/2 flatline
## [!!!][d60] - marginal

                ##------------------------------------------------------------------
                ## [+][d65] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 65) {
                    
                    tmp.hhol     <- NULL
                    tmp.phol     <- NULL
 
                ##------------------------------------------------------------------
                ## [***][d67] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 67) {
                    
                    load("040.003_ArimaOrderSearch_Dept04.10.18.34.56.67.79.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","va_m00","ea_m01","ea_m00","ha_m01","ha_m00","xm_m02","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("sb_m00","va_m00","ea_m01","ea_m00","ha_m01","ha_m00","xm_m02","xm_m01")]
                    
 ## [!!!][d71] - needs a review
 
                ##------------------------------------------------------------------
                ## [***][d72] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 72) {
                    
                    load("040.003_ArimaOrderSearch_Dept72.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m00","xm_m01","sb_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m00","xm_m01","sb_m00")]
                    
                ##------------------------------------------------------------------
                ## [?][d74] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 74) {
                    
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("fj_m00","td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("fj_m00","td_m01","td_m00","xm_m01")]
      
## [!!!][d77] - missings
## [!!!][d78] - missings
  
                ##------------------------------------------------------------------
                ## [+][d79] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 79) {
                    
                    load("040.003_ArimaOrderSearch_Dept04.10.18.34.56.67.79.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
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
                    
                    load("040.003_ArimaOrderSearch_Dept09.11.82.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m00","fj_m02","fj_m01","ld_m02","ld_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m00","fj_m02","fj_m01","ld_m02","ld_m01")]
 
 ## [!!!][d83] - needs a review
 ## [!!!][d85] - needs a review
 
                ##------------------------------------------------------------------
                ## [+][d87] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 87) {
                    
                    load("040.003_ArimaOrderSearch_Dept08.87.93.94.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- NULL
                    tmp.phol     <- NULL
                
                ##------------------------------------------------------------------
                ## [***][d90] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 90) {
                    
                    load("040.003_ArimaOrderSearch_Dept90.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","td_m01","td_m00","xm_m02","xm_m01","xm_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","td_m01","td_m00","xm_m02","xm_m01","xm_m00")]
                    
                ##------------------------------------------------------------------
                ## [***][d91] - ringing
                ##------------------------------------------------------------------
                } else if (tmp.dept == 91) {
                    
                    load("040.003_ArimaOrderSearch_Dept91.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [***][d92] - ringing
                ##------------------------------------------------------------------
                } else if (tmp.dept == 92) {
                    
                    load("040.003_ArimaOrderSearch_Dept92.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m02","xm_m01","xm_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m02","xm_m01","xm_m00")]

                ##------------------------------------------------------------------
                ## [***][d93] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 93) {
                   
                    load("040.003_ArimaOrderSearch_Dept08.87.93.94.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [d94] Add Mother's day flag - s07/s10/s12/s21/s22/
                ##------------------------------------------------------------------
                } else if (tmp.dept == 94) {
                    
                    load("040.003_ArimaOrderSearch_Dept08.87.93.94.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("va_m00","mo_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("va_m00","mo_m00")]
                    
                ##------------------------------------------------------------------
                ## [***][d95] - ringing
                ##------------------------------------------------------------------
                } else if (tmp.dept == 95) {
                    
                    load("040.003_ArimaOrderSearch_Dept95.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
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
 
 ## [!!!][d98] - needs a fit
 ## [!!!][d99] - N/A mostly missing
 
                ##------------------------------------------------------------------
                ## All Others
                ##------------------------------------------------------------------
                } else {
                    
                    tmp.hhol     <- NULL
                    tmp.phol     <- NULL
                    
                }
               
                ##------------------------------------------------------------------
                ## grab coefficients and do the fit
                ##------------------------------------------------------------------
                f.coef  <- c(paste("C",1:k,sep=""),paste("S",1:k,sep=""))
                tmp.fit <- calcFourierFit(ws, coeffs=f.coef, regs.hist=tmp.hhol, regs.proj=tmp.phol, k=40, h=39)
                
                ##------------------------------------------------------------------
                ## plot the results
                ##------------------------------------------------------------------
                pdf(tmp.filename)
                    plot(log10(pmax(10,c(tmp.fit$x,tmp.fit$forecast))), type="n", main=tmp.sdName, xlab="Time Index", ylab="Weekly Sales")
                    points(log10(pmax(10,tmp.fit$x)), type="b", pch=20, col="grey", lwd=2)
                    points(log10(pmax(10,c(tmp.fit$fitted, tmp.fit$forecast))), type="b", pch=1, col="red")
                dev.off()
			
                ##------------------------------------------------------------------
                ## return the fit
                ##------------------------------------------------------------------
                save(tmp.fit, file=paste("./TEST_FILES/",tmp.sdName,".Rdata",sep=""))   ## backup
                tmp.fit <- tmp.fit
			
            } else {
		
                ##------------------------------------------------------------------
                ## return a null fit
                ##------------------------------------------------------------------
                tmp.fit <- NULL
                save(tmp.fit, file=paste("./TEST_FILES/",tmp.sdName,".Rdata",sep=""))   ## backup
                tmp.fit <- NULL
			
            }
        }
    }
}
#names(vanilla.list) <- paste("SD_",uniqTestSd,sep="")

##------------------------------------------------------------------
## Save image
##------------------------------------------------------------------
##save(vanilla.list, file="042_VanillaFourier_TOP10TEST.Rdata")

##------------------------------------------------------------------
## Close connection
##------------------------------------------------------------------
sink()




