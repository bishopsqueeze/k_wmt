##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Use a the results of the order search to determine which of
##     the additional regressors might be useful in the search
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
minObs          <- 100

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
            
            ## grab the weekly sales data (floored at $10 b/c of box-cox)
            ws      <- tmp.hist$ws.min10
            
            ## define the regressors
            reg.dates   <- holiday.df[ (tmp.tr_fl == 1), c(grep("ea_", names(holiday.df)), grep("mo_",names(holiday.df))) ]
            reg.econ    <- tmp.dat[ (tmp.tr_fl == 1), c(19:23) ]
            
            # weight options
            #tmp.wgt     <- rep(1, length(ws))
            #tmp.wgt     <- tmp.wgt[ (tmp.tr_fl == 1) ]
            #tmp.wgt     <- exp(-((5:147)-147)^2/(52^2))
            
            ## perform the fit
            tmp.fit     <- calcGlmVariableSearch(ws, regs.hist=cbind(reg.dates, reg.econ), min.order=5, max.order=40, min.boxcox=0, max.boxcox=1, wgt=NULL)
            
		} else {
		
			tmp.fit <- NULL
			
		}
	}
    return(tmp.fit)
}


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
dept.aic <- tapply(aic.mat[,c("k")], aic.mat[,c("dept")], function(x){ceiling(mean(x, na.rm=TRUE))})

##------------------------------------------------------------------
## Save image
##------------------------------------------------------------------
##save(fourierVariable.list, file="041.001_GlmVariableSearch_20140328.Rdata")





