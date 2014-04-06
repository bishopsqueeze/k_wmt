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
## Load s/d data
##------------------------------------------------------------------
##load("005_walmartCombinedData_20140326.Rdata")
load("005_walmartCombinedData_20140403.Rdata")     ## backfilled the markdown data

##------------------------------------------------------------------
## Load the glm() variable selection list
##------------------------------------------------------------------
load("041.001_GlmVariableSearch_Min5_Max40_FactorsAll_HolidaysAll_WeightUNif_201400403.Rdata")

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
MaxK            <- 20

## minimum requirements for a fit
minObs          <- 90   ## down from 100

##------------------------------------------------------------------
## use sink() to monitor progress
##------------------------------------------------------------------
writeLines(c(""), "forecastFourierParallel.txt")
sink("forecastFourierParallel.txt", append=TRUE)

##------------------------------------------------------------------
## Loop over all of the test s/d combos and compute the forecast
##------------------------------------------------------------------
dept.list   <- as.integer(substr(uniqTestSd,4,5))

## do department-specific loops
for (j in 1:numTestDept) {

    loop.dept   <- uniqTestDept[j]
    loop.idx    <- which(dept.list %in% loop.dept)
    loop.list   <- list()
    
    loop.list   <- foreach(i=1:length(loop.idx)) %dopar% {
    #for (i in 1:length(loop.idx)) {
    
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
    
        ## weight vector
        tmp.wgt.exp <- exp(-((5:147)-147)^2/(52^2))
    
        ## number of original observations
        num.obs		<- sum(!is.na(tmp.hist$weekly_sales))

        ## define a filename for the plot
        tmp.folder		<- paste(wd, "/TEST/", substr(tmp.sd,4,5), "/", sep="")
        tmp.filename	<- paste(tmp.folder, tmp.sdName, ".ForecastFourierResults.pdf", sep="")
        dir.create(tmp.folder, showWarnings = FALSE)

        ##------------------------------------------------------------------
        ## basic stl projection
        ##------------------------------------------------------------------
        if ( (tmp.store > 0) & (tmp.dept == 7) ) {
            if ((num.obs > minObs) & (tmp.sd != "43_28")) {

                ## clear the fit dat & report progress
                tmp.fit <- NULL
                cat("Processing: SD = ", tmp.sdName, "\n")

                ## open the file for this s/d
                tmp.loadname <- paste("./OrderSearch_Arima/040.003_ArimaOrderSearch_AllRegs.",tmp.sdName,".Rdata",sep="")
                load(tmp.loadname)
                
                ## grab the harmonic order
                k       <- tmp.fit$k
                f.coef  <- c(paste("C",1:k,sep=""),paste("S",1:k,sep=""))
                
                ## grab the data
                ws	<- tmp.dat$ws.min10[ (tmp.tr_fl == 1) ]     ## min10 because of BoxCox transform
               
                ##------------------------------------------------------------------
                ## isoalte the regression variables
                ##------------------------------------------------------------------
                ## use the t-value to identify candidate pred
                tmp.tval     <- fourierRegressionGlm.list[[tmp.sdName]]$tval
                
                ## place a threshold on regressors
                tmp.tval.idx <- which( (abs(tmp.tval) >=  3))
                
                if (length(tmp.tval.idx) > 0) {
                    
                    ## isolate the potential variables
                    tmp.varlist  <- names(tmp.tval[tmp.tval.idx])
                    
                    ## prune Intercept and Fourier coefficients
                    if (length(grep("^[tSC]", tmp.varlist)) > 0) {
                        tmp.varlist <- tmp.varlist[ -grep("^[tSC]", tmp.varlist) ]     ## prune fourier coefficients
                    }
                    if (length(grep("Intercept", tmp.varlist)) > 0) {
                        tmp.varlist <- tmp.varlist[ -grep("Intercept", tmp.varlist) ]     ## prune fourier coefficients
                    }
                    
                    ##------------------------------------------------------------------
                    ## isolate the top-N holiday variables
                    ##------------------------------------------------------------------
                    var.holidays <- tmp.varlist[ grep("_", tmp.varlist) ]
                    if (length(var.holidays) > 0) {
                        var.holidays <- var.holidays[1:min(5,length(var.holidays))]
                    }
                    
                    ## load the holidays
                    if (length(var.holidays) == 0) {
                        tmp.hholidays <- NULL
                        tmp.pholidays <- NULL
                    } else if (length(var.holidays) == 1) {
                        tmp.hholidays           <- as.matrix(holiday.df[ (tmp.tr_fl == 1), var.holidays])
                        colnames(tmp.hholidays) <- var.holidays
                        tmp.pholidays           <- as.matrix(holiday.df[ (tmp.tr_fl == 0), var.holidays])
                        colnames(tmp.pholidays) <- var.holidays
                    } else {
                        tmp.hholidays           <- as.matrix(holiday.df[ (tmp.tr_fl == 1), var.holidays ])
                        tmp.pholidays           <- as.matrix(holiday.df[ (tmp.tr_fl == 0), var.holidays ])
                    }
                    
                    ##------------------------------------------------------------------
                    ## isolate the top-N economic variables
                    ##------------------------------------------------------------------
                    var.economic <- tmp.varlist[ grep("^n", tmp.varlist) ]
                    if (length(var.economic) > 0) {
                        var.economic <- var.economic[1:min(10,length(var.economic))]
                    }
                    
                    ## load the economic data
                    if ( length(var.economic) == 0 ) {
                        tmp.heconomic <- NULL
                        tmp.peconomic <- NULL
                    } else if ( length(var.economic) == 1 ) {
                        tmp.heconomic           <- as.matrix(tmp.dat[ (tmp.tr_fl == 1), var.economic])
                        tmp.peconomic           <- as.matrix(tmp.dat[ (tmp.tr_fl == 0), var.economic])
                        colnames(tmp.heconomic) <- var.economic
                        colnames(tmp.peconomic) <- var.economic
                    } else {
                        tmp.heconomic           <- as.matrix(tmp.dat[ (tmp.tr_fl == 1), var.economic ])
                        tmp.peconomic           <- as.matrix(tmp.dat[ (tmp.tr_fl == 0), var.economic ])
                    }
                    
                    
                    ##------------------------------------------------------------------
                    ## experimental overrides
                    ##------------------------------------------------------------------
                    
                    
                    if (tmp.dept == 5) {
                        tmp.hholidays     <- as.matrix(holiday.df[ (tmp.tr_fl == 1), c("ea_m00","md_m00","td_m00","xm_m01")])
                        tmp.pholidays     <- as.matrix(holiday.df[ (tmp.tr_fl == 0), c("ea_m00","md_m00","td_m00","xm_m01")])
                        #tmp.heconomic     <- NULL
                        #tmp.peconomic     <- NULL
                        k                 <- 26
                    }
                    
                    ## with c("td_m00","xm_m01") + k=fitted; slightly worse
                    if (tmp.dept == 7) {
                        tmp.hholidays     <- as.matrix(holiday.df[ (tmp.tr_fl == 1), c("td_m00","xm_m01")])
                        tmp.pholidays     <- as.matrix(holiday.df[ (tmp.tr_fl == 0), c("td_m00","xm_m01")])
                        #tmp.heconomic     <- NULL
                        #tmp.peconomic     <- NULL
                        k                 <- 26
                    }
                    
                    if (tmp.dept == 72) {
                        tmp.hholidays     <- as.matrix(holiday.df[ (tmp.tr_fl == 1), c("td_m00","xm_m01")])
                        tmp.pholidays     <- as.matrix(holiday.df[ (tmp.tr_fl == 0), c("td_m00","xm_m01")])
                        tmp.heconomic     <- NULL
                        tmp.peconomic     <- NULL
                        k                 <- 26
                    }
                    
                    
                } else {
                    
                    ## load null data for no regressors
                    tmp.hholidays <- NULL
                    tmp.pholidays <- NULL
                    tmp.heconomic <- NULL
                    tmp.peconomic <- NULL
                    
                }
                
                ## combine the regressors
                tmp.hregs <- cbind(tmp.hholidays, tmp.heconomic)
                tmp.pregs <- cbind(tmp.pholidays, tmp.peconomic)

                ##------------------------------------------------------------------
                ## do the fit
                ##------------------------------------------------------------------
                new.fit <- calcFourierFit(ws, coeffs=f.coef, regs.hist=tmp.hregs, regs.proj=tmp.pregs, k=40, h=39)
                
                ##------------------------------------------------------------------
                ## plot the results
                ##------------------------------------------------------------------
                pdf(tmp.filename)
                    plot(log10(pmax(10,c(new.fit$x,new.fit$forecast))), type="n", main=tmp.sdName, xlab="Time Index", ylab="Weekly Sales")
                    points(log10(pmax(10,new.fit$x)), type="b", pch=20, col="grey", lwd=2)
                    points(log10(pmax(10,c(new.fit$fitted, new.fit$forecast))), type="b", pch=1, col="red")
                dev.off()
			
                ##------------------------------------------------------------------
                ## return the fit
                ##------------------------------------------------------------------
                new.fit <- new.fit
			
            } else {
		
                ##------------------------------------------------------------------
                ## return a null fit
                ##------------------------------------------------------------------
                new.fit <- NULL
            }
            
            ## save an intermediate copy of the fit
            tmp.savename <- paste("./TEST_FILES/042.002_Forecast.",tmp.sdName,".Rdata",sep="")
            save(new.fit, file=tmp.savename)
        }
     }
}
#names(vanilla.list) <- paste("SD_",uniqTestSd,sep="")

##------------------------------------------------------------------
## Save image
##------------------------------------------------------------------
#save(vanilla.list, file="042.001_Forecast_MinK20_S021_04032014.Rdata")

##------------------------------------------------------------------
## Close connection
##------------------------------------------------------------------
sink()




