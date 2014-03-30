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
	if ( (tmp.store > 0) & (tmp.dept == 5) ) {
		if (num.obs >= minObs) {
            
            ws      <- tmp.hist$ws.min10

            ##------------------------------------------------------------------
            ## [+][d05]
            ##------------------------------------------------------------------
            if (tmp.dept == 5) {
                tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m01","sb_m00","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]

            ##------------------------------------------------------------------
            ## [+][d07] - "td_m00","td_p01" for separation
            ##------------------------------------------------------------------
            } else if (tmp.dept == 7) {
                tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00","td_m00","td_p01")]

            ##------------------------------------------------------------------
            ## [+][d72] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 72) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00", "td_m00", "xm_m01")]
                
            ##------------------------------------------------------------------
            ## [catch-all]
            ##------------------------------------------------------------------
            } else {
                tmp.hhol <- NULL
            }
            
            tmp.fit <- calcFourierOrder(ws, id=tmp.sdName, regs.hist=tmp.hhol, min.order=15, max.order=25, min.boxcox=0, max.boxcox=1)
            
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
save(list.names, list.k, aic.mat, fourierOrderArima.list, file="040.003_ArimaOrderSearch_Dept05.Rdata")





