##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Compute a forecast based on a set of sine/cosine waves and
##     a series of date flags.
##  2. The fourier order is defined by the prior order search script
##     (but there is a floor b/c os some issues)
##  3. The dates are based on a visual scan of some of the fit
##     results ... and so need to be optimized
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
load("005_walmartCombinedData_20140314.Rdata")                              ## raw data
load("./VariableSearch/041_FourierVariableSearch_All_20140328.Rdata")      ## optimal fourier coefficients
orders.list <- fourierVariable.list


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
## Main
##------------------------------------------------------------------

##------------------------------------------------------------------
## Process the fit file
##------------------------------------------------------------------
orderCoef.list  <- list()
fit.names       <- names(orders.list)

for (i in 1:length(fit.names)) {

    ## get the AIC data, but set a low-limit to the order == 5
    tmp.name    <- fit.names[i]
    tmp.coef    <- orders.list[[tmp.name]]$coef
    #tmp.res     <- orders.list[[tmp.name]]$res
    #tmp.res     <- tmp.res[ tmp.res[,1] >=5 , ]
    
    ## then pull off the order with the minimum in-sample AICc
    #tmp.k       <- tmp.res[ which(tmp.res[,2] == min(tmp.res[,2])) , 1][1]
    #tmp.k                           <- tmp.res[2]
    orderCoef.list[[tmp.name]]$k    <- orders.list[[tmp.name]]$k
    orderCoef.list[[tmp.name]]$coef <- names(tmp.coef)[2:length(tmp.coef)]
    
}


##------------------------------------------------------------------
## store results in a list
##------------------------------------------------------------------
variable.list		<- list()

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
	
    tmp.dat     <- tmp.dat[ , -which(colnames(tmp.dat) == "time")]  ## drop the time column
    
	tmp.tr_fl	<- tmp.dat$tr_fl
	tmp.wgt		<- ifelse(tmp.dat$isholiday, 5, 1)
	tmp.hist	<- tmp.dat[ (tmp.tr_fl == 1) , ]			## historical data
	tmp.proj	<- tmp.dat[ (tmp.tr_fl == 0) , ]			## projection data

    ## grab fourier coefficients
    tmp.coef    <- orderCoef.list[[tmp.sdName]]$coef
    
	## number of original observations
	num.obs		<- sum(!is.na(tmp.hist$weekly_sales))

	## define a filename for the plot
	tmp.folder		<- paste(wd, "/ForecastVariableFourier_0328_Arima/", substr(tmp.sd,4,5), "/", sep="")
	tmp.filename	<- paste(tmp.folder, tmp.sdName, ".ForecastFourierResults.pdf", sep="")
	dir.create(tmp.folder, showWarnings = FALSE)

	##------------------------------------------------------------------
	## basic stl projection
	##------------------------------------------------------------------
	if ( (tmp.store > 0) & (tmp.dept > 0) ) {
		if ((num.obs > minObs) & (tmp.sd != "43_28")) {
            if ( !is.null(orderCoef.list[[tmp.sdName]]) ) {

                ## report progress
                cat("Processing: SD = ", tmp.sdName, "\n")

                ## grab the data
                ws	<- tmp.dat$ws.min10[ (tmp.tr_fl == 1) ]     ## min10 because of BoxCox transform
                
                ##------------------------------------------------------------------
                ## In this fit, we're just using the vanilla regression factors to
                ## generate the forecast.
                ##------------------------------------------------------------------
                
                ## adjust fit based on number of coefficients selected ???
                holiday.index   <- c(grep("[_]", tmp.coef), grep("time", tmp.coef))
                fourier.index   <- grep("^[SC]", tmp.coef)
    
                holiday.names   <- tmp.coef[ holiday.index ]
                fourier.names   <- tmp.coef[ fourier.index ]
                factor.names    <- tmp.coef[ -c(holiday.index, fourier.index) ]
             
                tmp.hregs           <- data.frame(  holiday.df[ (tmp.tr_fl == 1), which(colnames(holiday.df) %in% holiday.names) ],
                                                    tmp.dat[ (tmp.tr_fl == 1), which(colnames(tmp.dat) %in% factor.names)])
                colnames(tmp.hregs) <- c(holiday.names, factor.names)
                
                
                tmp.pregs           <- data.frame(  holiday.df[ (tmp.tr_fl == 0), which(colnames(holiday.df) %in% holiday.names) ],
                                                    tmp.dat[ (tmp.tr_fl == 0), which(colnames(tmp.dat) %in% factor.names)])
                colnames(tmp.pregs) <- c(holiday.names, factor.names)


                tmp.wgt     <- 1 + 0*tmp.wgt[(tmp.tr_fl == 1) ]
                
                #tmp.fit	<- calcFourierNoArima(ws, coeffs=fourier.names, regs.hist=tmp.hregs, regs.proj=tmp.pregs, wgt=tmp.wgt, k=40, h=39)
                tmp.fit	<- calcFourierFit(ws, coeffs=fourier.names, regs.hist=tmp.hregs, regs.proj=tmp.pregs, k=40, h=39)
               
                ##------------------------------------------------------------------
                ## plot the results
                ##------------------------------------------------------------------
                pdf(tmp.filename)
                    plot(c(tmp.fit$x,tmp.fit$forecast), type="n", main=tmp.sdName, xlab="Time Index", ylab="Weekly Sales")
                    points(tmp.fit$x, type="b", pch=20, col="grey", lwd=2)
                    points(c(tmp.fit$fitted, tmp.fit$forecast), type="b", pch=1, col="red")
                dev.off()
			
                ## save the results
                variable.list[[tmp.sdName]] <- tmp.fit
			
            } else {
		
                ## will hit if num.obs <= 50
                variable.list[[tmp.sdName]] <- NULL
			
            }
        }
    }
}

##------------------------------------------------------------------
## Save image
##------------------------------------------------------------------
save(variable.list, file="043_VariableFourier_Arima_20140328.Rdata")





