##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Use a simple linear regression to identify the optimal number
##     of fourier terms to use in a fit.
##  2. *** NOTE *** There is some noise in the results of this fit
##     b/c sometimes the oder 1 is selected ... which generates the
##     smalles AICc, but non-sensible forecasts.  Need to amend this.
##------------------------------------------------------------------
options(warn=1)

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(forecast)
library(foreach)
library(doMC)
library(MASS)

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
##load("005_walmartCombinedData_20140314.Rdata")
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

## minimum requirements for a fit
minObs          <- 50

##------------------------------------------------------------------
## Loop over all of the test s/d combos and compute the forecast
##------------------------------------------------------------------
fourierRegressionRq.list <- foreach(i=1:numTestSd) %dopar% {

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

	##------------------------------------------------------------------
	## compute a glm-based order search of fourier series
	##------------------------------------------------------------------
	if ( (tmp.store > 0) & (tmp.dept > 0) ) {
		if (num.obs >= minObs) {
            
            ws      <- tmp.hist$ws.min10
            tmp.fit <- calcRqOrderSearch(ws, min.order=5, max.order=30, min.boxcox=0, max.boxcox=1)
            
		} else {
            
            tmp.fit <- NULL
            
		}
	}
    return(tmp.fit)
}

## add names to the list
names(fourierRegressionRq.list) <- paste("SD_",uniqTestSd,sep="")

##------------------------------------------------------------------
## Save image
##------------------------------------------------------------------
save(fourierRegressionRq.list, file="040.002_RqOrderSearch_20140326.Rdata")





