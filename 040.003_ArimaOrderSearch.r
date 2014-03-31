##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Use the auto.arima function in the forecast library to identify
##     the optimal fourier order to use for fitting a particular
##     time series.
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
## Load data
##------------------------------------------------------------------
load("005_walmartCombinedData_20140326.Rdata")

##------------------------------------------------------------------
## Remove superfluous items
##------------------------------------------------------------------
rm(train, test, comb, sd.list)

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
minOrder        <- 15
maxOrder        <- 30

## minimum requirements for a fit
minObs          <- 100

##------------------------------------------------------------------
## use sink() to monitor progress
##------------------------------------------------------------------
#vanilla.list    <- list()
writeLines(c(""), "arimaOrderSearchParallel.txt")
## report progress
sink("log.txt", append=TRUE)

##------------------------------------------------------------------
## Loop over all of the test s/d combos and compute the forecast
##------------------------------------------------------------------
fourierOrderArima.list <- foreach(i=1:numTestSd) %dopar% {

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
    
    ## add holiday flags
    holiday.df$sb_m01                           <- Lag(holiday.df$sb_m00,-1)
    holiday.df$sb_m01[is.na(holiday.df$sb_m01)] <- 0

	## number of original observations
	num.obs		<- sum(!is.na(tmp.hist$weekly_sales))

	##------------------------------------------------------------------
	## basic  projection
	##------------------------------------------------------------------
	if ( (tmp.store > 0) & (tmp.dept == 91) ) {
		if (num.obs >= minObs) {

            cat("Processing: SD = ", tmp.sdName, "\n")

            ## grab the sales data
            ws <- tmp.hist$ws.min10

            ##------------------------------------------------------------------
            ## [***][d01] -
            ##------------------------------------------------------------------
            if (tmp.dept == 1) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","va_m00","ea_m01","ea_m00","ha_m01","ha_m00","xm_m02","xm_m01")]
                
            ##------------------------------------------------------------------
            ## [***][d02] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 2) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                
            ##------------------------------------------------------------------
            ## [***][d03] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 3) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]

            ##------------------------------------------------------------------
            ## [***][d05]
            ##------------------------------------------------------------------
            } else if (tmp.dept == 5) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("sb_m01","sb_m00","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]

            ##------------------------------------------------------------------
            ## [***][d07] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 7) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00","td_m00","td_p01")]

            ##------------------------------------------------------------------
            ## [***][d16] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 16) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                
            ##------------------------------------------------------------------
            ## [***][d18] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 18) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","va_m00","ea_m00","ha_m00")]
                
            ##------------------------------------------------------------------
            ## [***][d38] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 38) {
                tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]

            ##------------------------------------------------------------------
            ## [][d40] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 40) {
                tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                
            ##------------------------------------------------------------------
            ## [***][d72] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 72) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00", "td_m00", "xm_m01")]
                
            ##------------------------------------------------------------------
            ## [***][d90] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 90) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","td_m01","td_m00","xm_m02","xm_m01","xm_m00")]
                
            ##------------------------------------------------------------------
            ## [***][d91] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 91) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]

            ##------------------------------------------------------------------
            ## [***][d92] - (similar to d90)
            ##------------------------------------------------------------------
            } else if (tmp.dept == 92) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m02","xm_m01","xm_m00")]
                
            ##------------------------------------------------------------------
            ## [***][d95] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 95) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("fj_m01","fj_m00","xm_m01","xm_m00")]
                
            ##------------------------------------------------------------------
            ## [catch-all]
            ##------------------------------------------------------------------
            } else {
                tmp.hhol <- NULL
            }
            
            tmp.fit <- calcFourierOrder(ws, id=tmp.sdName, regs.hist=tmp.hhol, min.order=minOrder, max.order=maxOrder, min.boxcox=0, max.boxcox=1)
            
		} else {

            tmp.fit <- NULL
			
		}
	}
    #return(tmp.fit)
}

##------------------------------------------------------------------
## Load the results into a compact forms
##------------------------------------------------------------------
list.names  <- unlist(lapply(fourierOrderArima.list, function(x){x$id}))
list.k      <- unlist(lapply(fourierOrderArima.list, function(x){x$k}))

## matrix of k(s)
cnt <- 1
aic.mat <- matrix(, nrow=length(list.names), ncol=(maxOrder-minOrder+1))
for (i in 1:length(fourierOrderArima.list)) {
    if( !is.null(fourierOrderArima.list[[i]]) ) {
        aic.mat[cnt,] <- fourierOrderArima.list[[i]]$res[,2]
        cnt <- cnt + 1
    }
}


##------------------------------------------------------------------
## Save image
##------------------------------------------------------------------
save(list.names, list.k, aic.mat, fourierOrderArima.list, file="040.003_ArimaOrderSearch_Dept91.Rdata")

##------------------------------------------------------------------
## Close connection
##------------------------------------------------------------------
sink()


