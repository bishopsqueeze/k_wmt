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
rm(train, test, stores, features, comb, sd.list)

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
minObs          <- 90

##------------------------------------------------------------------
## Loop over all of the test s/d combos and compute the forecast
##------------------------------------------------------------------
fourierRegressionGlm.list <- foreach(i=1:numTestSd) %dopar% {

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
            tmp.fit <- calcGlmOrderSearch(ws, min.order=5, max.order=40, min.boxcox=0, max.boxcox=1)
            
		} else {
            
            tmp.fit <- NULL
            
		}
	}
    return(tmp.fit)
}

## add names to the list
names(fourierRegressionGlm.list) <- paste("SD_",uniqTestSd,sep="")

##------------------------------------------------------------------
## Generate a table of the AIC results
##------------------------------------------------------------------

## define and load the matrix
aic.mat <- matrix(, nrow=length(fourierRegressionGlm.list), ncol=30-5+1)
for (i in 1:nrow(aic.mat)) {
    if ( !is.null(fourierRegressionGlm.list[[i]]$res[,2]) ) {
        aic.mat[i, ] <- fourierRegressionGlm.list[[i]]$res[,2]
    }
}
rownames(aic.mat) <- names(fourierRegressionGlm.list)
colnames(aic.mat) <- paste("X",seq(5,30,1),sep="")

## add store/dept/k columns
aic.mat <- cbind(   aic.mat,
                    store=as.numeric(substr(rownames(aic.mat),4,5)),
                    dept=as.numeric(substr(rownames(aic.mat),7,8)),
                    k=NA
                )

## append the order (k) to the matrix
aic.mat[ which(rownames(aic.mat) %in% names(unlist(lapply(fourierRegressionGlm.list, function(x){x$k})))), c("k")] <- unlist(lapply(fourierRegressionGlm.list, function(x){x$k}))

##------------------------------------------------------------------
## Extract department-level average orders
##------------------------------------------------------------------
#dept.aic <- tapply(aic.mat[,c("k")], aic.mat[,c("dept")], function(x){ceiling(mean(x, na.rm=TRUE))})

##------------------------------------------------------------------
## Save image
##------------------------------------------------------------------
#save(dept.aic, fourierRegressionGlm.list, file="040.001_GlmOrderSearch_20140326.Rdata")





