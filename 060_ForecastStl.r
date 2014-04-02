##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Compute a forecast using a stl() decomposition
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
load("005_walmartCombinedData_20140314.Rdata")


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
minObs          <- 50

##------------------------------------------------------------------
## Main
##------------------------------------------------------------------

## store results in a list
stl.list		<- list()

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
	tmp.folder		<- paste(wd, "/ForecastStl/", substr(tmp.sd,1,2), "/", sep="")
	tmp.filename	<- paste(tmp.folder, tmp.sdName, ".ForecastStlResults.pdf", sep="")
	dir.create(tmp.folder, showWarnings = FALSE)

	## report progress
	cat("Processing: SD = ", tmp.sdName, "\n")

	##------------------------------------------------------------------
	## basic stl projection
	##------------------------------------------------------------------
	if ( (tmp.store >= 1) ) {
		if (num.obs > minObs) {

			## compute a basic stl projection
			ws		<- tmp.dat$ws.min10[ (tmp.tr_fl == 1) ]     ## min10 because of BoxCox transform
			tmp.fit	<- calcStlFit(ws, h=39)
			
			## plot the results
			pdf(tmp.filename)
                plot(c(tmp.fit$x,tmp.fit$forecast), type="n", main=tmp.sdName, xlab="Time Index", ylab="Weekly Sales")  ## null plot
                points(tmp.fit$x, type="b", pch=20, col="grey", lwd=2)
				points(c(tmp.fit$fitted, tmp.fit$forecast), type="b", pch=1, col="red")
            dev.off()
			
			## save the results
			stl.list[[tmp.sdName]] <- tmp.fit
			
		} else {
		
			## will hit if num.obs <= 50
			stl.list[[tmp.sdName]] <- NULL
			
		}
	}
}

##------------------------------------------------------------------
## Save image
##------------------------------------------------------------------
save(stl.list, file="020_ForecastStl_20140314.Rdata")





