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
##load("005_walmartCombinedData_20140326.Rdata")
load("005_walmartCombinedData_20140403.Rdata")     ## backfilled the markdown data

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
minOrder        <- 22
maxOrder        <- 30

## minimum requirements for a fit
minObs          <- 90

##------------------------------------------------------------------
## Load the glm() variable selection list
##------------------------------------------------------------------
load("041.001_GlmVariableSearch_Min5_Max40_FactorsAll_HolidaysAll_WeightUNif_201400403.Rdata")

##------------------------------------------------------------------
## use sink() to monitor progress
##------------------------------------------------------------------
writeLines(c(""), "arimaOrderSearchParallel.txt")
sink("log.txt", append=TRUE)

##------------------------------------------------------------------
## Loop over all of the test s/d combos and compute the forecast
##------------------------------------------------------------------
dept.list   <- as.integer(substr(uniqTestSd,4,5))

## do department-specific loops
for (j in 1:numTestDept) {
#for (j in 1:1) {
    
    loop.dept   <- uniqTestDept[j]
    loop.idx    <- which(dept.list == loop.dept)
    loop.list   <- list()
    
    loop.list   <- foreach(i=1:length(loop.idx)) %dopar% {
    
        ## grab s/d parameters from the clean.list()
        
        #tmp.sd		<- as.character(droplevels(uniqTestSd[i]))
        tmp.sd      <- uniqTestSd[loop.idx[i]]
        
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
        ## basic  projection
        ##------------------------------------------------------------------
        if ( (tmp.store == 1) & (tmp.dept > 0) ) {
            if (num.obs >= minObs) {

            cat("Processing: SD = ", tmp.sdName, "\n")

            ##------------------------------------------------------------------
            ## identify variables to test
            ##------------------------------------------------------------------
            
            ## use the t-value to identify candidate pred
            tmp.tval     <- fourierRegressionGlm.list[[tmp.sdName]]$tval
            
            ## isolate the top-N holiday variables
            tmp.tval.hol <- tmp.tval[ grep("_", names(tmp.tval)) ]
            tmp.tval.hol <- tmp.tval.hol[order(abs(tmp.tval.hol), decreasing=TRUE)]
            tmp.tval.hol <- tmp.tval.hol[1:min(5,length(tmp.tval.hol))]
            
            ## place a threshold on regressors
            tmp.tval.idx <- which( (abs(tmp.tval) > 2.0))
        
            ## isolate variable names
            tmp.varlist <- fourierRegressionGlm.list[[tmp.sdName]]$coef
            tmp.varlist <- names(tmp.varlist)[ tmp.tval.idx ]
            tmp.varlist <- tmp.varlist[ -grep("^[tSC]", tmp.varlist) ]     ## prune fourier coefficients
            tmp.varlist <- tmp.varlist[ -grep("Intercept", tmp.varlist) ]     ## prune fourier coefficients
            
            ## identify the preferred order ... the theoretical optimum is ~26 given the
            ## frequency of the data ... but there may be cases where the best fit
            ## deviates from the theoretical value
            tmp.k       <- fourierRegressionGlm.list[[tmp.sdName]]$coef
            
            ## search around the natural harmonic
            minOrder    <- 20
            maxOrder    <- 26
            
            ## grab the sales data
            ws <- tmp.hist$ws.min10

            ## extract the holiday regressors
            if (length(grep("_", tmp.varlist)) == 0) {
                tmp.holidays <- holiday.df[ (tmp.tr_fl == 1), c("xm_m01")]      ## no holidays (use x-mas as a default)
            } else {
                #tmp.holidays <- holiday.df[ (tmp.tr_fl == 1), tmp.varlist[grep("_", tmp.varlist)] ]
                tmp.holidays <- as.matrix(holiday.df[ (tmp.tr_fl == 1), names(tmp.tval.hol[!is.na(tmp.tval.hol)]) ])
                colnames(tmp.holidays) <- names(tmp.tval.hol[!is.na(tmp.tval.hol)])
            }

            ## extract the economic regressors
            if (length(grep("^n", tmp.varlist)) == 0) {
                tmp.economic <- NULL
            } else {
                tmp.economic <- as.matrix(tmp.dat[ (tmp.tr_fl == 1), tmp.varlist[grep("^n", tmp.varlist)] ])
                colnames(tmp.economic) <- tmp.varlist[grep("^n", tmp.varlist)]
            }
            
            ## combine the regressors
            if (is.null(tmp.economic)) {
                tmp.regs <- tmp.holidays
            } else {
                tmp.regs <- cbind(tmp.holidays, tmp.economic)
                
            }
            
            ##------------------------------------------------------------------
            ## perform the fit
            ##------------------------------------------------------------------
            tmp.fit   <- calcFourierOrder(ws, id=tmp.sdName, regs.hist=tmp.regs, min.order=minOrder, max.order=maxOrder, min.boxcox=0, max.boxcox=1)
            #tmp.fit.2 <- calcFourierOrder(tmp.fit$residuals, id=tmp.sdName, regs.hist=tmp.economic, min.order=1, max.order=2, min.boxcox=0, max.boxcox=1, lambda=1)
            #tmp.fit <- calcFourierOrder(ws, id=tmp.sdName, regs.hist=tmp.regs, min.order=26, max.order=26, min.boxcox=0, max.boxcox=1)
            
            } else {

            tmp.fit <- NULL
			
            }
            
            ## save an intermediate copy of the fit
            tmp.filename <- paste("./OrderSearch_Arima/040.003_ArimaOrderSearch_AllRegs.",tmp.sdName,".Rdata",sep="")
            save(tmp.fit, file=tmp.filename)
            
        } ## end of the fit loop
        
    } ## end of %dopar% loop

    ##------------------------------------------------------------------
    ## Load the results into a compact forms
    ##------------------------------------------------------------------
    list.names          <- unlist(lapply(loop.list, function(x){x$id}))
    list.k              <- unlist(lapply(loop.list, function(x){x$k}))
    names(loop.list)    <- uniqTestSd[which(dept.list %in% loop.dept)]

    ##------------------------------------------------------------------
    ## Save the department-level image [FAILED]
    ##------------------------------------------------------------------
    #tmp.deptname <- ifelse(loop.dept<10,paste("0",loop.dept,sep=""),loop.dept)
    #tmp.filename <- paste("040.003_ArimaOrderSearch__AllRegs_Dept",loop.dept,".Rdata",sep="")
    #save(list.names, list.k, loop.list, file=tmp.filename)
    
} ## end of main loop

##------------------------------------------------------------------
## Close connection
##------------------------------------------------------------------
sink()


