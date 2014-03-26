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
library(MASS)

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
load("005_walmartCombinedData_20140314.Rdata")
load("040.01_FourierRegressionSearch_20140314.Rdata")   ## optimal fourier orders

##------------------------------------------------------------------
## Remove superfluous items
##------------------------------------------------------------------
rm(train, test, stores, features, comb, sd.list)

##------------------------------------------------------------------
## Process the oprimal fourier orders file
##------------------------------------------------------------------
order.list  <- list()
fit.names   <- names(fourierRegression.list)

for (i in 1:length(fit.names)) {
    
    ## get the AIC data, but set a low-limit to the order == 5
    tmp.name    <- fit.names[i]
    tmp.res     <- fourierRegression.list[[tmp.name]]$res
    tmp.res     <- tmp.res[ tmp.res[,1] >=5 , ]
    
    ## then pull off the order with the minimum in-sample AICc
    tmp.k       <- tmp.res[ which(tmp.res[,2] == min(tmp.res[,2])) , 1][1]
    order.list[[tmp.name]]$k    <- tmp.k
}

##------------------------------------------------------------------
## Source Utilities
##------------------------------------------------------------------
source("/Users/alexstephens/Development/kaggle/walmart/code/999_UtilityFunctions.r")


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

## store results in a list
fourierVariable.list		<- list()

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

	## number of original observations
	num.obs		<- sum(!is.na(tmp.hist$weekly_sales))

	## define a filename for the plot
	#tmp.folder		<- paste(wd, "/FourierRegressionOrder_0326/", substr(tmp.sd,1,2), "/", sep="")
	#tmp.filename	<- paste(tmp.folder, tmp.sdName, ".FourierRegressionOrder.pdf", sep="")
	#dir.create(tmp.folder, showWarnings = FALSE)

	## report progress
	cat("Processing: SD = ", tmp.sdName, "\n")

	##------------------------------------------------------------------
	## basic  projection
	##------------------------------------------------------------------
	if ( (tmp.store >= 1) & (tmp.dept >= 1) ) {
		if (num.obs >= minObs) {
            
            ##
            ##num.sim <- 30
            ws      <- tmp.hist$ws.min10[ (tmp.tr_fl == 1) ]
            k       <- order.list[[tmp.sdName]]$k
            ## get the order
            
            #tmp.fit <- calcFourierRegression(ws, max.order=num.sim)
            reg.hist    <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
            reg.proj    <- holiday.df[ (tmp.tr_fl == 0), c("td_m01","td_m00","xm_m01")]
            tmp.fit     <- calcFourierVariableSearch(ws, regs.hist=reg.hist, regs.proj=reg.proj, k=k, h=39)
        
        
			## plot the results
			#pdf(tmp.filename)
            #    plot(c(tmp.fit$x), type="n", main=tmp.sdName, xlab="Time Index", ylab="Weekly Sales")  ## null plot
            #    points(tmp.fit$x, type="b", pch=20, col="grey", lwd=2)
			#	#points(c(tmp.fit$fitted, tmp.fit$forecast), type="b", pch=1, col="red")
            #    points(c(tmp.fit$fitted), type="b", pch=1, col="red")
            #dev.off()
			
			## save the results
            fourierVariable.list[[tmp.sdName]] <- tmp.fit
			
		} else {
		
			## will hit if num.obs <= 50
			fourierVariable.list[[tmp.sdName]] <- NULL
			
		}
	}
}

##------------------------------------------------------------------
## Save image
##------------------------------------------------------------------
##save(fourierVariable.list, file="040.01_FourierVariableSearch_20140326.Rdata")





