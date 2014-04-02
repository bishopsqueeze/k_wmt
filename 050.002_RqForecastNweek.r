##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Generate a forecast based on a simple averaging of the
##     historical data.  These forecasts are used to fill-in when a
##     a more sophisticated task will not work (generally due to a
##     large number of missing observations)
##------------------------------------------------------------------
options(warn=1)

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(forecast)
library(quantreg)

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
load("005_walmartCombinedData_20140326.Rdata")          ## raw data

##------------------------------------------------------------------
## Constants
##------------------------------------------------------------------

## all data
uniqStores	<- uniq.list$uniqStores
uniqDept	<- uniq.list$uniqDept
uniqSd		<- uniq.list$uniqSd
numStores	<- uniq.list$numStores
numDept		<- uniq.list$numDept
numSd		<- uniq.list$numSd

## test data
uniqTestStores	<- uniq.list$uniqTestStores
uniqTestDept	<- uniq.list$uniqTestDept
uniqTestSd		<- uniq.list$uniqTestSd
numTestStores	<- uniq.list$numTestStores
numTestDept		<- uniq.list$numTestDept
numTestSd		<- uniq.list$numTestSd

## time
numWeek		<- 52
minTime		<- 5
maxTime		<- 186
interpLim	<- 20	## max number of missing points for interpolation

##########################################################################################

##------------------------------------------------------------------
## Grab the unique test dates and nweeks from the combined data
##------------------------------------------------------------------
proj.data   <- data.frame( date=as.Date(unique(comb$date)),
						   nweek=as.numeric(format(as.Date(unique(comb$date), format="%Y-%m-%d"), format="%V")),
						   time=seq(from=minTime, to=maxTime, by=1))
						 
##------------------------------------------------------------------
## Compute a lookup table to avoid fits with small numbers of observations
##------------------------------------------------------------------
tr 				<- train
trStoreChar		<- ifelse(tr$store < 10, paste("0", tr$store,sep=""), tr$store)
trDeptChar		<- ifelse(tr$dept < 10, paste("0", tr$dept,sep=""), tr$dept)
tr$sd_idx		<- as.factor(paste(trStoreChar,"_", trDeptChar,sep=""))
tr.tbl			<- table(tr$sd_idx)

##******************************************************************
## Compute de-trended seasonal factors based on historical data alone
## and use those factors (plus any trend) to forecast weekly_sales
##******************************************************************

## intialize variables
nweek.list	<- list()
nweek.vec	<- seq(from=1, to=numWeek, by=1)

##------------------------------------------------------------------
## loop over all store/dept (s/d) combinations and compute projections
## ... but remember you are only considering those s/d listed in the 
## test dataset ... which is different than the full combined data
##------------------------------------------------------------------
for (i in 1:numTestSd) {

	## grab s/d parameters
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

	## Identify the good sales data (i.e., not NA(s)) in the dataset
	tmp.idx		<- !is.na(tmp.hist$weekly_sales)
	
	## define a filename for the plot
	tmp.folder		<- paste(wd, "/RqNweek/", substr(tmp.sd,4,5), "/", sep="")
	tmp.filename	<- paste(tmp.folder, tmp.sdName, ".nweekResults.pdf", sep="")
	dir.create(tmp.folder, showWarnings = FALSE)

	## report progress
	cat("Processing: SD = ", tmp.sdName, "\n")

	##--------------------------------------------------------------
	## initialize 52-week vector in each loop (with NAs)
	##--------------------------------------------------------------
	meanObservation.vec		<- NA*vector("numeric", length=numWeek)
	seasonalFactor.vec		<- NA*vector("numeric", length=numWeek)
	
	##--------------------------------------------------------------
	## Remove prior iteration items to avoid possible confusion
	##--------------------------------------------------------------	
	if (exists("tmp.rq")) { rm(tmp.rq) }
	proj.data$trend				<- 0
	proj.data$sf				<- 0
	proj.data$weekly_sales.f	<- 0

	##--------------------------------------------------------------
	## Assign project data preliminaries
	##--------------------------------------------------------------
	proj.data$sd_idx	<- as.factor(tmp.sd)
	
	##--------------------------------------------------------------
	## do a regression only if there are enough datapoints
	##--------------------------------------------------------------
	if ( sum(tmp.idx) == 0 ) {
	
		##**************************************************************
		## no data -- compute a null projection
		##**************************************************************
		proj.data$sf				<- 0
		proj.data$trend				<- 0
		proj.data$weekly_sales.f	<- 0

	} else if ( sum(tmp.idx) <= 50 ) {	
	
		##**************************************************************
		## limited data -- compute the average 52 week profile
		##**************************************************************
		
		##--------------------------------------------------------------
		## aggregate de-trended sales by nweek (for use in computing the seasonal facotr)
		##--------------------------------------------------------------
		tmp.sdData	<- aggregate(weekly_sales ~ nweek + sd_idx, data=tmp.hist, FUN=mean)

		##--------------------------------------------------------------
		## align output to a 52-week year (some s/d may have missings nweeks)
		##--------------------------------------------------------------
		meanObservation.vec[ tmp.sdData$nweek ] <- tmp.sdData$weekly_sales
		
		##--------------------------------------------------------------
		## <for now> replace missings with the median value
		##--------------------------------------------------------------
		badIndex <- is.na(meanObservation.vec)
		if (sum(badIndex) > 0) {
			meanObservation.vec[ badIndex ] <- median(meanObservation.vec, na.rm=TRUE)
		} 
		
		##--------------------------------------------------------------
		## compute seasonal factor
		## - in this instance, we are using an additive term that will be
		##   applied to the regression-based trend, so no normalization 
		##   is needed (so it is redundant)
		##--------------------------------------------------------------
		seasonalFactor.vec	<- meanObservation.vec	
	
		##--------------------------------------------------------------
		## compute the projection
		##--------------------------------------------------------------
		proj.data$sf				<- seasonalFactor.vec[proj.data$nweek]
		proj.data$trend				<- 0
		proj.data$weekly_sales.f	<- proj.data$trend + proj.data$sf
									
	} else {
	
		##**************************************************************
		## sufficient data for regression
		##**************************************************************
		
		##--------------------------------------------------------------
		## quantile regresion on the median vs. the time index
		##--------------------------------------------------------------
		tmp.rq		<- rq(ws.min00 ~ time, tau=.5, data=tmp.hist, method="br")
		#tmp.lm		<- lm(ws.min00 ~ time, data=tmp.hist)

		##--------------------------------------------------------------
		## de-trend the data
		##--------------------------------------------------------------
		tmp.hist$ws.nt	<- (tmp.hist$ws.min00 - tmp.rq$fitted.values)

		##--------------------------------------------------------------
		## aggregate de-trended sales by nweek (for use in computing the seasonal facotr)
		##--------------------------------------------------------------
		tmp.sdData	<- aggregate(ws.nt ~ nweek + sd_idx, data=tmp.hist, FUN=mean)
	
		##--------------------------------------------------------------
		## align output to a 52-week year (some s/d may have missings nweeks)
		##--------------------------------------------------------------
		meanObservation.vec[ tmp.sdData$nweek ] <- tmp.sdData$ws.nt
	
		##--------------------------------------------------------------
		## <for now> replace missings with the median value
		##--------------------------------------------------------------
		badIndex	<- is.na(meanObservation.vec)
		if (sum(badIndex) > 0) {
			meanObservation.vec[ badIndex ] <- median(meanObservation.vec, na.rm=TRUE)
		} 

		##--------------------------------------------------------------
		## compute seasonal factor
		## - in this instance, we are using an additive term that will be
		##   applied to the regression-based trend, so no normalization 
		##   is needed (so it is redundant)
		##--------------------------------------------------------------
		seasonalFactor.vec	<- meanObservation.vec

		##--------------------------------------------------------------
		## compute the projection over the historical and future times
		##--------------------------------------------------------------
		proj.data$sf				<- seasonalFactor.vec[proj.data$nweek]
		proj.data$trend				<- predict(tmp.rq, proj.data)
		proj.data$weekly_sales.f	<- proj.data$trend + proj.data$sf
		
	}
    
	## compute the mean absolute error
	if ( !is.null( proj.data$weekly_sales.f ) ) { 
		mae	<- sum(abs(tmp.hist$ws.c - proj.data$weekly_sales.f[1:143]))/length(tmp.hist$ws.c)
	}

	## return the original vector, the in-sample, out-of-sample, and box-cox parameter
	nweek.list[[tmp.sdName]] <- list(	x=tmp.hist$weekly_sales,
										fitted=proj.data$weekly_sales.f[1:143], 
										forecast=proj.data$weekly_sales.f[144:182], 
										lambda=NULL, mae=mae)
										
    tmp.fit <- nweek.list[[tmp.sdName]]
    ## plot the results
    pdf(tmp.filename)
        plot(log10(pmax(10,c(tmp.fit$x,tmp.fit$forecast))), type="n", main=tmp.sdName, xlab="Time Index", ylab="Weekly Sales")
        points(log10(pmax(10,tmp.fit$x)), type="b", pch=20, col="grey", lwd=2)
        points(log10(pmax(10,c(tmp.fit$fitted, tmp.fit$forecast))), type="b", pch=1, col="red")
    dev.off()


if ((i %% 100) == 0) { cat("iteration %d", i, "\n") }
}


##------------------------------------------------------------------
## Save image
##------------------------------------------------------------------
save(nweek.list, file="050.002_RqForecastNweek_20140326.Rdata")

