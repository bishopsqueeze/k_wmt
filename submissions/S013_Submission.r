##------------------------------------------------------------------
## The purpose of this script is to:
##	1.
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
## Load fit results
##------------------------------------------------------------------
load("999_CompareModels_S013_20140328.Rdata")

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
interpLim       <- 20	## max number of missing points for interpolation

##########################################################################################

##------------------------------------------------------------------
## <function> :: Compute scoring metric
##------------------------------------------------------------------
calcWmaeScore	<- function(myForecast, myBenchmark, myHoliday) {
	
	## create the weight vector
	w		<- 1*(!myHoliday) + 5*(myHoliday)
	wmae	<- (w %*% abs(myForecast-myBenchmark))/sum(w)

return(wmae)	
}

##########################################################################################

##------------------------------------------------------------------
## <function> :: Calculate seasonal factor forecast
##------------------------------------------------------------------
calcForecast <- function(myData, myDates, myResults, myMean, myMedian, myLm, myLmMulti, myStl, myTslm, myQrMulti, myFourier, myVanilla, myNweek) {
	

    ##--------------------------------------------------------------
	## pre-allocate a matrix
	##--------------------------------------------------------------
	myForecast	<- matrix(NA, nrow=nrow(myData), ncol=3)
	myType		<- vector("character", length=nrow(myData))
    
	##--------------------------------------------------------------
	## loop over each row in the test file and gather data
	##--------------------------------------------------------------
	for (i in 1:nrow(myData)) {

		## <DEBUG> 
		if ((i %% 100) == 0) { cat("Iteration=",i,"\n") }
		
		## Gather identifying and debugging data
		tmpStore	<- myData$store[i]
		tmpDept		<- myData$dept[i]
		tmpNweek	<- myData$nweek[i]
		tmpDate		<- as.Date(myData$date[i])
		tmpSdIdx	<- myData$sd_idx[i]		
		tmpSdName	<- paste("SD_",tmpSdIdx,sep="")
		tmpId		<- paste(tmpStore, tmpDept, tmpDate, sep="_")


        ##  vanilla > nweek ... (lmmulti > lm > tslm > median > mean > qrmulti > stl > fourier)
        if (!is.null(myVanilla[[tmpSdName]])) {
            
            tmpProj	 <- c(myVanilla[[tmpSdName]]$fitted, myVanilla[[tmpSdName]]$forecast)
            dateIdx	 <- which(as.Date(myDates) == tmpDate)
            tmpFc	 <- tmpProj[dateIdx]
            tmpType	 <- "vanilla"
            
        } else if (!is.null(myNweek[[tmpSdName]])) {
            
            tmpProj	 <- c(myNweek[[tmpSdName]]$fitted, myNweek[[tmpSdName]]$forecast)
            dateIdx	 <- which(as.Date(myDates) == tmpDate)
            tmpFc	 <- tmpProj[dateIdx]
            tmpType	 <- "nweek"

        } else if (!is.null(myLmMulti[[tmpSdName]])) {
        
            tmpProj	 <- c(myLmMulti[[tmpSdName]]$fitted, myLmMulti[[tmpSdName]]$forecast)
            dateIdx	 <- which(as.Date(myDates) == tmpDate)
            tmpFc	 <- tmpProj[dateIdx]
            tmpType	 <- "lmmulti"
        
        } else if (!is.null(myLm[[tmpSdName]])) {
            
            tmpProj	 <- c(myLm[[tmpSdName]]$fitted, myLm[[tmpSdName]]$forecast)
            dateIdx	 <- which(as.Date(myDates) == tmpDate)
            tmpFc	 <- tmpProj[dateIdx]
            tmpType	 <- "lm"
            
        } else if (!is.null(myMedian[[tmpSdName]])) {
            
            tmpProj	 <- c(myMedian[[tmpSdName]]$fitted, myMedian[[tmpSdName]]$forecast)
            dateIdx	 <- which(as.Date(myDates) == tmpDate)
            tmpFc	 <- tmpProj[dateIdx]
            tmpType	 <- "median"
            
        } else if (!is.null(myMean[[tmpSdName]])) {
            
            tmpProj	 <- c(myMean[[tmpSdName]]$fitted, myMean[[tmpSdName]]$forecast)
            dateIdx	 <- which(as.Date(myDates) == tmpDate)
            tmpFc	 <- tmpProj[dateIdx]
            tmpType	 <- "mean"

        } else if (!is.null(myQrMulti[[tmpSdName]])) {
            
            tmpProj	 <- c(myQrMulti[[tmpSdName]]$fitted, myQrMulti[[tmpSdName]]$forecast)
            dateIdx	 <- which(as.Date(myDates) == tmpDate)
            tmpFc	 <- tmpProj[dateIdx]
            tmpType	 <- "qrmulti"
            
        } else if (!is.null(myStl[[tmpSdName]])) {
                
            tmpProj	 <- c(myStl[[tmpSdName]]$fitted, myStl[[tmpSdName]]$forecast)
            dateIdx	 <- which(as.Date(myDates) == tmpDate)
            tmpFc	 <- tmpProj[dateIdx]
            tmpType	 <- "stl"
                
        } else if (!is.null(myTslm[[tmpSdName]])) {
            
            tmpProj	 <- c(myTslm[[tmpSdName]]$fitted, myTslm[[tmpSdName]]$forecast)
            dateIdx	 <- which(as.Date(myDates) == tmpDate)
            tmpFc	 <- tmpProj[dateIdx]
            tmpType	 <- "tslm"

        } else if (!is.null(myFourier[[tmpSdName]])) {
                
            ## CAPPED/FLOORED FOR DEBUGGING
            tmpProj	 <- pmax(min(myFourier[[tmpSdName]]$x), pmin( max(myFourier[[tmpSdName]]$x), c(myFourier[[tmpSdName]]$fitted, myFourier[[tmpSdName]]$forecast) ))
            dateIdx	 <- which(as.Date(myDates) == tmpDate)
            tmpFc	 <- tmpProj[dateIdx]
            tmpType	 <- "fourier"
                
        } else {
            
            ## no hit
            tmpFc	 <- 0
			tmpType	 <- "null"
            
        }
				
		## load the forecast
		myForecast[i, ] <- c(tmpStore, tmpDept, tmpFc)
		myType[i] 		<- tmpType
	}
	
	##--------------------------------------------------------------
    ## recast as a dataframe and clean the headers
	##--------------------------------------------------------------
    finForecast 			<- data.frame(myForecast, date=as.Date(myData$date), type=myType)
	colnames(finForecast)   <- c("store.f", "dept.f", "weekly_sales.f", "date.f", "type.f")
		
return(finForecast)
}

##########################################################################################

##------------------------------------------------------------------
## compute the out-of-sample forecasts
##------------------------------------------------------------------

forecast.test	<- subset(comb, t_id == "TE")
forecast 		<- calcForecast(    myData=forecast.test,
                                    myDates=holiday.df$date,
                                    myResults=res.matrix,
                                    myMean=mean.list,
                                    myMedian=median.list,
                                    myLm=lm.list,
                                    myLmMulti=lmMulti.list,
                                    myStl=stl.list,
                                    myTslm=tslm.list,
                                    myQrMulti=qrMulti.list,
                                    myFourier=fourier.list,
                                    myVanilla=vanilla.list,
                                    myNweek=nweek.list)

##------------------------------------------------------------------
## compute the in-sample forecasts
##------------------------------------------------------------------
forecast.train	<- subset(comb, t_id == "TR")
forecast.is 	<- calcForecast(    myData=forecast.train,
                                    myDates=holiday.df$date,
                                    myResults=res.matrix,
                                    myMean=mean.list,
                                    myMedian=median.list,
                                    myLm=lm.list,
                                    myLmMulti=lmMulti.list,
                                    myStl=stl.list,
                                    myTslm=tslm.list,
                                    myQrMulti=qrMulti.list,
                                    myFourier=fourier.list,
                                    myVanilla=vanilla.list,
                                    myNweek=nweek.list)

##------------------------------------------------------------------
## Append the forcast to the *out-of-sample* data file & add an ID
##------------------------------------------------------------------
combOutput		<- cbind(test, forecast)
combOutput$Id	<- paste(combOutput$store, combOutput$dept, combOutput$date, sep="_")

##------------------------------------------------------------------
## Append the forcast to the *in-sample* data file & add an ID
##------------------------------------------------------------------
combOutput.is		<- cbind(train, forecast.is)
combOutput.is$Id	<- paste(combOutput.is$store, combOutput.is$dept, combOutput.is$date, sep="_")


##------------------------------------------------------------------
## Compute benchmarks (using in-sample data)
##------------------------------------------------------------------

## all-zeros & uniform holiday benchmark	
allZeros.flat	<- calcWmaeScore(myForecast=rep(0,nrow(combOutput.is)), myBenchmark=combOutput.is$weekly_sales, myHoliday=rep(1,nrow(combOutput.is))) 

## all-zeros & competition holiday benchmark	
allZeros.score	<- calcWmaeScore(myForecast=rep(0,nrow(combOutput.is)), myBenchmark=combOutput.is$weekly_sales, myHoliday=combOutput.is$isholiday) 

## current forecast
forecast.score	<- calcWmaeScore(myForecast=combOutput.is$weekly_sales.f, myBenchmark=combOutput.is$weekly_sales, myHoliday=combOutput.is$isholiday) 


##------------------------------------------------------------------
## Create a file for plotting both the history & the projection
##------------------------------------------------------------------
testComp						<- combOutput[ , c("store", "dept", "date", "isholiday", "weekly_sales.f")]
names(testComp)[ncol(testComp)] <- c("weekly_sales")

## Append train and test data to produce a comparison
hp	<- rbind(        train[ , c("store", "dept", "date", "isholiday", "weekly_sales")],
				  testComp[ , c("store", "dept", "date", "isholiday", "weekly_sales")])
				  
hp	<- hp[ order(hp$store, hp$dept, hp$date), ]

## Save to an Rdata file
S013_HistoryAndProjection <- hp
save(S013_HistoryAndProjection, file="S013_HistoryAndProjection.Rdata")


##------------------------------------------------------------------
## Save submission and other diagnostics
##------------------------------------------------------------------

## Create the submission file (should have the following columns)
res	<- data.frame(Id=combOutput$Id, Weekly_Sales=combOutput$weekly_sales.f)

## Ouput should have 115064 rows and 2 columns
dim(res)

## Write submission data to a .cev
write.csv(res, file="S013_Submission.csv", row.names=FALSE)

## Save information on the biggest misses
misses.df <- data.frame(    abserr=abs(combOutput.is$weekly_sales.f-combOutput.is$weekly_sales),
                            pred=combOutput.is$weekly_sales.f,
                            act=combOutput.is$weekly_sales,
                            isholiday=combOutput.is$isholiday,
                            store=combOutput.is$store,
                            dept=combOutput.is$dept)
						
misses.df <- misses.df[ order(-misses.df$abserr), ]
write.csv(misses.df, file="S013_Misses.csv")


##------------------------------------------------------------------
## Save the workspace
##------------------------------------------------------------------
save.image(file="S013_Submission.Rdata")


