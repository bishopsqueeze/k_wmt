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
## Load the glm() variable selection list
##------------------------------------------------------------------
load("./VariableSearch/041_FourierVariableSearch_All_20140328.Rdata")

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
MaxK            <- 20

## minimum requirements for a fit
minObs          <- 90   ## down from 100


##------------------------------------------------------------------
## Process the fit file
##------------------------------------------------------------------
#order.list     <- fourierRegression.list
#fit.names       <- names(order.list)
#
#for (i in 1:length(fit.names)) {
#
#    ## get the AIC data, but set a low-limit to the order == 15
#    tmp.name                    <- fit.names[i]
#    tmp.res                     <- order.list[[tmp.name]]$res
#    tmp.k                       <- tmp.res[ (tmp.res[,1] >= 15) & (tmp.res[,1] <= 25), ]
#    order.list[[tmp.name]]$k    <- tmp.k[which(tmp.k[,2] == min(tmp.k[,2])), 1][1]
#
#}

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
##for (i in 1:numTestSd) {
    
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
    
    tmp.wgt.exp <- exp(-((5:147)-147)^2/(52^2))
    
	## number of original observations
	num.obs		<- sum(!is.na(tmp.hist$weekly_sales))

    ## add new holiday flags
    holiday.df$sb_m01                           <- Lag(holiday.df$sb_m00,-1)
    holiday.df$sb_m01[is.na(holiday.df$sb_m01)] <- 0
    holiday.df$ld_m02                           <- Lag(holiday.df$ld_m01,-1)
    holiday.df$ld_m02[is.na(holiday.df$ld_m02)] <- 0
    holiday.df$ld_m03                           <- Lag(holiday.df$ld_m02,-1)
    holiday.df$ld_m03[is.na(holiday.df$ld_m03)] <- 0
    holiday.df$fj_m02                           <- Lag(holiday.df$fj_m01,-1)
    holiday.df$fj_m02[is.na(holiday.df$fj_m02)] <- 0

	## define a filename for the plot
	tmp.folder		<- paste(wd, "/TEST/", substr(tmp.sd,4,5), "/", sep="")
	tmp.filename	<- paste(tmp.folder, tmp.sdName, ".ForecastFourierResults.pdf", sep="")
	dir.create(tmp.folder, showWarnings = FALSE)

	##------------------------------------------------------------------
	## basic stl projection
	##------------------------------------------------------------------
	if ( (tmp.store > 0) & (tmp.dept > 0) ) {
		if ((num.obs > minObs) & (tmp.sd != "43_28")) {
            #if ( !is.null(order.list[[tmp.sdName]]) ) {

                cat("Processing: SD = ", tmp.sdName, "\n")

                ## grab variables to test
                tmp.varlist <- fourierVariable.list[[tmp.sdName]]$coef
                tmp.varlist <- names(tmp.varlist)[ c(grep("_",names(tmp.varlist )), grep("^n",names(tmp.varlist))) ]

                ## grab the data
                ws	<- tmp.dat$ws.min10[ (tmp.tr_fl == 1) ]     ## min10 because of BoxCox transform

                ## set a harmonic order lower limit
                k   <- 15
                
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
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept01.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","va_m00","ea_m01","ea_m00","ha_m01","ha_m00","xm_m02","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("sb_m00","va_m00","ea_m01","ea_m00","ha_m01","ha_m00","xm_m02","xm_m01")]

                ##------------------------------------------------------------------
                ## [+][d02] - s22/s23/s29/s39
                ##------------------------------------------------------------------
                } else if (tmp.dept == 2) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept02.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [+][d03] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 3) {
 
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept03.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [+][d04] - similar response as d05
                ##------------------------------------------------------------------
                } else if (tmp.dept == 4) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept04.10.18.34.56.67.79.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("sb_m00","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [***][d05] - dates important
                ##------------------------------------------------------------------
                } else if (tmp.dept == 5) {
                    
                    #load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept05.Rdata")
                    #tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m01","sb_m00","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                    #tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("sb_m01","sb_m00","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                    load("040.003_ArimaOrderSearch_Dept05.07.72.92_Experiment.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.idx  <- grep("_", tmp.varlist)
                    if (tmp.store == 28) {
                        tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("sb_m01","sb_m00","fj_m00","td_m00","xm_m01", tmp.varlist[tmp.idx])]
                        tmp.phol <- holiday.df[ (tmp.tr_fl == 0), c("sb_m01","sb_m00","fj_m00","td_m00","xm_m01", tmp.varlist[tmp.idx])]
                    } else {
                        tmp.hreg <- tmp.dat[ (tmp.tr_fl == 1) , tmp.varlist[-tmp.idx]]
                        tmp.preg <- tmp.dat[ (tmp.tr_fl == 0) , tmp.varlist[-tmp.idx]]
                        tmp.hhol <- cbind(holiday.df[ (tmp.tr_fl == 1), c("sb_m01","sb_m00","fj_m00","td_m00","xm_m01", tmp.varlist[tmp.idx])], tmp.hreg)
                        tmp.phol <- cbind(holiday.df[ (tmp.tr_fl == 0), c("sb_m01","sb_m00","fj_m00","td_m00","xm_m01", tmp.varlist[tmp.idx])], tmp.preg)
                    }
                    
                ##------------------------------------------------------------------
                ## [~][d06] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 6) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept06.27.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("mo_m00","fj_m00","td_m00","xm_m02","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("mo_m00","fj_m00","td_m00","xm_m02","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [***][d07] - spike dom; added "td_m00","td_p01" for separation
                ##------------------------------------------------------------------
                } else if (tmp.dept == 7) {
                    
                    #load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept07.Rdata")
                    #k <- list.k[ which(list.names %in% tmp.sdName) ]
                    #tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00","td_m00","td_p01")]
                    #tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00","td_m00","td_p01")]
                    load("040.003_ArimaOrderSearch_Dept05.07.72.92_Experiment.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.idx  <- grep("_", tmp.varlist)
                    if (tmp.store == 28) {
                        tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00", "td_m00", "xm_m01", tmp.varlist[tmp.idx])]
                        tmp.phol <- holiday.df[ (tmp.tr_fl == 0), c("sb_m00", "td_m00", "xm_m01", tmp.varlist[tmp.idx])]
                    } else {
                        tmp.hreg <- tmp.dat[ (tmp.tr_fl == 1) , tmp.varlist[-tmp.idx]]
                        tmp.preg <- tmp.dat[ (tmp.tr_fl == 0) , tmp.varlist[-tmp.idx]]
                        tmp.hhol <- cbind(holiday.df[ (tmp.tr_fl == 1), c("sb_m00", "td_m00", "xm_m01", tmp.varlist[tmp.idx])], tmp.hreg)
                        tmp.phol <- cbind(holiday.df[ (tmp.tr_fl == 0), c("sb_m00", "td_m00", "xm_m01", tmp.varlist[tmp.idx])], tmp.preg)
                    }
                    
                ##------------------------------------------------------------------
                ## [+][d08] - excess s02/s17/s23/s24/s25/s35/s39/s40/s41 (caps???)
                ##------------------------------------------------------------------
                } else if (tmp.dept == 8) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept08.87.93.94.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [+][d09] - s33(use historical)
                ##------------------------------------------------------------------
                } else if (tmp.dept == 9) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept09.11.82.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [+][d10] - holidays may not add value
                ##------------------------------------------------------------------
                } else if (tmp.dept == 10) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept04.10.18.34.56.67.79.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [+][d11] - memorial/july fourth spikes; some temperature correlations ?
                ##------------------------------------------------------------------
                } else if (tmp.dept == 11) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept09.11.82.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("md_m00","fj_m01","fj_m00","td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("md_m00","fj_m01","fj_m00","td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [***][d12] - no real spikes
                ##------------------------------------------------------------------
                } else if (tmp.dept == 12) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept12.20.21.22.24.25.26.28.30.31.33.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [***][d13] - no real spikes
                ##------------------------------------------------------------------
                } else if (tmp.dept == 13) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept13.32.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [***][d14] - no real spikes
                ##------------------------------------------------------------------
                } else if (tmp.dept == 14) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept14.46.55.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","va_m00","td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("sb_m00","va_m00","td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [***][d16] - no real spikes k=20???
                ##------------------------------------------------------------------
                } else if (tmp.dept == 16) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept16.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00")]

                ##------------------------------------------------------------------
                ## [+][d17]
                ##------------------------------------------------------------------
                } else if (tmp.dept == 17) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept17.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [+][d18] - hyperseasonal
                ##------------------------------------------------------------------
                } else if (tmp.dept == 18) {
                    
                    #load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept04.10.18.34.56.67.79.Rdata")
                    load("040.003_ArimaOrderSearch_Dept18_Experiment.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hreg <- tmp.dat[ (tmp.tr_fl == 1) , c("ndtemp","ndfuel","nmd1","nmd2","nmd3","nmd4","nmd5")]
                    tmp.preg <- tmp.dat[ (tmp.tr_fl == 0) , c("ndtemp","ndfuel","nmd1","nmd2","nmd3","nmd4","nmd5")]
                    tmp.hhol <- cbind(tmp.hreg, holiday.df[ (tmp.tr_fl == 1), c("ea_m02","ea_m01","ea_m00")]) ## c("sb_m00","va_m00","ea_m00","ha_m00")
                    tmp.phol <- cbind(tmp.preg, holiday.df[ (tmp.tr_fl == 0), c("ea_m02","ea_m01","ea_m00")])
        
## [!!!][d19] - marginal

                ##------------------------------------------------------------------
                ## [+~][d20] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 20) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept12.20.21.22.24.25.26.28.30.31.33.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    
                ##------------------------------------------------------------------
                ## [+~][d21] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 21) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept12.20.21.22.24.25.26.28.30.31.33.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00")]
                    
                ##------------------------------------------------------------------
                ## [+~][d22] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 22) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept12.20.21.22.24.25.26.28.30.31.33.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00")]
                    
                ##------------------------------------------------------------------
                ## [+][d23] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 23) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept23.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    
                ##------------------------------------------------------------------
                ## [+~][d24] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 24) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept12.20.21.22.24.25.26.28.30.31.33.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [+~][d25] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 25) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept12.20.21.22.24.25.26.28.30.31.33.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    
                ##------------------------------------------------------------------
                ## [+~][d26] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 26) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept12.20.21.22.24.25.26.28.30.31.33.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [+~][d27] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 27) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept06.27.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("sb_m00","td_m01","td_m00","xm_m01")]

                ##------------------------------------------------------------------
                ## [+~][d28] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 28) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept12.20.21.22.24.25.26.28.30.31.33.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
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
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept12.20.21.22.24.25.26.28.30.31.33.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]

                ##------------------------------------------------------------------
                ## [+~][d31] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 31) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept12.20.21.22.24.25.26.28.30.31.33.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","va_m00","ea_m01","ea_m00","md_m00","ld_m02","ld_m01","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("sb_m00","va_m00","ea_m01","ea_m00","md_m00","ld_m02","ld_m01","xm_m01")]

                ##------------------------------------------------------------------
                ## [+][d32] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 32) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept13.32.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","ea_m00","md_m00","fj_m00")]
                    
                ##------------------------------------------------------------------
                ## [+~][d33] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 33) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept12.20.21.22.24.25.26.28.30.31.33.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","ea_m00","md_m00","ld_m03","ld_m02","ld_m01","td_m01","td_m00","xm_m02","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("sb_m00","ea_m00","md_m00","ld_m03","ld_m02","ld_m01","td_m01","td_m00","xm_m02","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [+][d34] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 34) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept04.10.18.34.56.67.79.Rdata")
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
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept38.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
## [!!!][d39] - skip
                ##------------------------------------------------------------------
                ## [***][d40] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 40) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept40.Rdata")
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
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept14.46.55.Rdata")
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
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept14.46.55.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m02","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m02","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [***][d56] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 56) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept04.10.18.34.56.67.79.Rdata")
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
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept04.10.18.34.56.67.79.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","va_m00","ea_m01","ea_m00","ha_m01","ha_m00","xm_m02","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("sb_m00","va_m00","ea_m01","ea_m00","ha_m01","ha_m00","xm_m02","xm_m01")]
                    
 ## [!!!][d71] - needs a review
 
                ##------------------------------------------------------------------
                ## [***][d72] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 72) {
                    
                    
                    #load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept72.Rdata")
                    load("040.003_ArimaOrderSearch_Dept72_Experiment.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    
                    tmp.idx  <- grep("_", tmp.varlist)
                    if (tmp.store == 28) {
                        tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00", "td_m00", "xm_m01", tmp.varlist[tmp.idx])]
                        tmp.phol <- holiday.df[ (tmp.tr_fl == 0), c("sb_m00", "td_m00", "xm_m01", tmp.varlist[tmp.idx])]
                    } else {
                        tmp.hreg <- tmp.dat[ (tmp.tr_fl == 1) , tmp.varlist[-tmp.idx]]
                        tmp.preg <- tmp.dat[ (tmp.tr_fl == 0) , tmp.varlist[-tmp.idx]]
                        tmp.hhol <- cbind(holiday.df[ (tmp.tr_fl == 1), c("sb_m00", "td_m00", "xm_m01", tmp.varlist[tmp.idx])], tmp.hreg)
                        tmp.phol <- cbind(holiday.df[ (tmp.tr_fl == 0), c("sb_m00", "td_m00", "xm_m01", tmp.varlist[tmp.idx])], tmp.preg)
                    }
                    #tmp.hreg <- tmp.dat[ (tmp.tr_fl == 1) , c("ndtemp","ndfuel","nmd1","nmd2","nmd3","nmd4","nmd5")]
                    #tmp.preg <- tmp.dat[ (tmp.tr_fl == 0) , c("ndtemp","ndfuel","nmd1","nmd2","nmd3","nmd4","nmd5")]
                    #tmp.hhol <- cbind(tmp.hreg, holiday.df[ (tmp.tr_fl == 1), c("td_m00","xm_m01","sb_m00")])
                    #tmp.phol <- cbind(tmp.preg, holiday.df[ (tmp.tr_fl == 0), c("td_m00","xm_m01","sb_m00")])
                    
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
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept04.10.18.34.56.67.79.Rdata")
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
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept09.11.82.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m00","fj_m02","fj_m01","ld_m02","ld_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m00","fj_m02","fj_m01","ld_m02","ld_m01")]
 
 ## [!!!][d83] - needs a review
 ## [!!!][d85] - needs a review
 
                ##------------------------------------------------------------------
                ## [+][d87] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 87) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept08.87.93.94.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- NULL
                    tmp.phol     <- NULL
                
                ##------------------------------------------------------------------
                ## [***][d90] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 90) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept90.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","td_m01","td_m00","xm_m02","xm_m01","xm_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("ea_m01","td_m01","td_m00","xm_m02","xm_m01","xm_m00")]
                    
                ##------------------------------------------------------------------
                ## [***][d91] - ringing
                ##------------------------------------------------------------------
                } else if (tmp.dept == 91) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept91.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [***][d92] - ringing
                ##------------------------------------------------------------------
                } else if (tmp.dept == 92) {
                    
                    #load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept92.Rdata")
                    #k <- list.k[ which(list.names %in% tmp.sdName) ]
                    #tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m02","xm_m01","xm_m00")]
                    #tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m02","xm_m01","xm_m00")]
                    load("040.003_ArimaOrderSearch_Dept05.07.72.92_Experiment.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.idx  <- grep("_", tmp.varlist)
                    if (tmp.store == 28) {
                        tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m02","xm_m01","xm_m00", tmp.varlist[tmp.idx])]
                        tmp.phol <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m02","xm_m01","xm_m00", tmp.varlist[tmp.idx])]
                    } else {
                        tmp.hreg <- tmp.dat[ (tmp.tr_fl == 1) , tmp.varlist[-tmp.idx]]
                        tmp.preg <- tmp.dat[ (tmp.tr_fl == 0) , tmp.varlist[-tmp.idx]]
                        tmp.hhol <- cbind(holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m02","xm_m01","xm_m00", tmp.varlist[tmp.idx])], tmp.hreg)
                        tmp.phol <- cbind(holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m02","xm_m01","xm_m00", tmp.varlist[tmp.idx])], tmp.preg)
                    }
                    
                ##------------------------------------------------------------------
                ## [***][d93] -
                ##------------------------------------------------------------------
                } else if (tmp.dept == 93) {
                   
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept08.87.93.94.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
                    
                ##------------------------------------------------------------------
                ## [d94] Add Mother's day flag - s07/s10/s12/s21/s22/
                ##------------------------------------------------------------------
                } else if (tmp.dept == 94) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept08.87.93.94.Rdata")
                    k <- list.k[ which(list.names %in% tmp.sdName) ]
                    tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("va_m00","mo_m00")]
                    tmp.phol     <- holiday.df[ (tmp.tr_fl == 0), c("va_m00","mo_m00")]
                    
                ##------------------------------------------------------------------
                ## [***][d95] - ringing
                ##------------------------------------------------------------------
                } else if (tmp.dept == 95) {
                    
                    load("./OrderSearch_Arima/040.003_ArimaOrderSearch_Base_Dept95.Rdata")
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
                    k            <- 15      ## order default
                }
               
                ##------------------------------------------------------------------
                ## grab coefficients and do the fit
                ##------------------------------------------------------------------
                k       <- max(MaxK,k)
                f.coef  <- c(paste("C",1:k,sep=""),paste("S",1:k,sep=""))
                tmp.fit <- calcFourierFit(ws, coeffs=f.coef, regs.hist=tmp.hhol, regs.proj=tmp.phol, k=40, h=39)
                
                ##------------------------------------------------------------------
                ## plot the results
                ##------------------------------------------------------------------
                #pdf(tmp.filename)
                #    plot(log10(pmax(10,c(tmp.fit$x,tmp.fit$forecast))), type="n", main=tmp.sdName, xlab="Time Index", ylab="Weekly Sales")
                #    points(log10(pmax(10,tmp.fit$x)), type="b", pch=20, col="grey", lwd=2)
                #    points(log10(pmax(10,c(tmp.fit$fitted, tmp.fit$forecast))), type="b", pch=1, col="red")
                #dev.off()
			
                ##------------------------------------------------------------------
                ## return the fit
                ##------------------------------------------------------------------
                #save(tmp.fit, file=paste("./TEST_FILES/",tmp.sdName,".Rdata",sep=""))   ## backup
                tmp.fit <- tmp.fit
			
            } else {
		
                ##------------------------------------------------------------------
                ## return a null fit
                ##------------------------------------------------------------------
                tmp.fit <- NULL
                #save(tmp.fit, file=paste("./TEST_FILES/",tmp.sdName,".Rdata",sep=""))   ## backup
                tmp.fit <- NULL
			
            }
            #}
    }
}
names(vanilla.list) <- paste("SD_",uniqTestSd,sep="")

##------------------------------------------------------------------
## Save image
##------------------------------------------------------------------
save(vanilla.list, file="042.001_Forecast_MinK20_S021_04032014.Rdata")

##------------------------------------------------------------------
## Close connection
##------------------------------------------------------------------
sink()




