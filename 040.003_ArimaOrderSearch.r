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
minOrder        <- 5
maxOrder        <- 40

## minimum requirements for a fit
minObs          <- 100

##------------------------------------------------------------------
## Main
##------------------------------------------------------------------

##------------------------------------------------------------------
## Loop over all of the test s/d combos and compute the forecast
##------------------------------------------------------------------
#fourierOrderArima.list <- foreach(i=1:numTestSd) %dopar% {
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
	#tmp.folder		<- paste(wd, "/ForecastLmMulti/", substr(tmp.sd,1,2), "/", sep="")
	#tmp.filename	<- paste(tmp.folder, tmp.sdName, ".ForecastLmMultiResults.pdf", sep="")
	#dir.create(tmp.folder, showWarnings = FALSE)

	## report progress
	#cat("Processing: SD = ", tmp.sdName, "\n")

	##------------------------------------------------------------------
	## basic  projection
	##------------------------------------------------------------------
	if ( (tmp.store >= 1) & (tmp.dept == 72) ) {
		if (num.obs >= minObs) {
            
            ws      <- tmp.hist$ws.min10
            tmp.fit <- calcFourierOrder(ws,  min.order=18, max.order=22, min.boxcox=0, max.boxcox=1)
	
			## plot the results
			#pdf(tmp.filename)
            #    plot(c(tmp.fit$x), type="n", main=tmp.sdName, xlab="Time Index", ylab="Weekly Sales")  ## null plot
            #    points(tmp.fit$x, type="b", pch=20, col="grey", lwd=2)
			#	#points(c(tmp.fit$fitted, tmp.fit$forecast), type="b", pch=1, col="red")
            #    points(c(tmp.fit$fitted), type="b", pch=1, col="red")
            #dev.off()
			
			## save the results
            #fourierOrder.list[[tmp.sdName]] <- tmp.fit
			
		} else {
		
			## will hit if num.obs <= minObs
			#fourierOrder.list[[tmp.sdName]] <- NULL
            tmp.fit <- NULL
			
		}
	}
    return(tmp.fit)
}

##------------------------------------------------------------------
## Save image
##------------------------------------------------------------------
##save(fourierOrder.list, file="040_FourierOrderSearch_20140314.Rdata")





