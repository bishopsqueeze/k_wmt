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
minOrder        <- 15
maxOrder        <- 20

## minimum requirements for a fit
minObs          <- 90

##------------------------------------------------------------------
## Load the glm() variable selection list
##------------------------------------------------------------------
load("./VariableSearch/041_FourierVariableSearch_All_20140328.Rdata")

##------------------------------------------------------------------
## use sink() to monitor progress
##------------------------------------------------------------------
#vanilla.list    <- list()
writeLines(c(""), "arimaOrderSearchParallel.txt")
## report progress
sink("log.txt", append=TRUE)

##------------------------------------------------------------------
## Loop over all of the test s/d combos and compute the forecast
##------------------------------------------------------------------
fourierOrderArima.list <- list()
fourierOrderArima.list <- foreach(i=1:numTestSd) %dopar% {
    #for (i in 1:numTestSd) {
    
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
    
    tmp.wgt.exp <- exp(-((5:147)-147)^2/(52^2))
    
    ## add new holiday flags
    holiday.df$sb_m01                           <- Lag(holiday.df$sb_m00,-1)
    holiday.df$sb_m01[is.na(holiday.df$sb_m01)] <- 0
    holiday.df$ld_m02                           <- Lag(holiday.df$ld_m01,-1)
    holiday.df$ld_m02[is.na(holiday.df$ld_m02)] <- 0
    holiday.df$ld_m03                           <- Lag(holiday.df$ld_m02,-1)
    holiday.df$ld_m03[is.na(holiday.df$ld_m03)] <- 0
    holiday.df$fj_m02                           <- Lag(holiday.df$fj_m01,-1)
    holiday.df$fj_m02[is.na(holiday.df$fj_m02)] <- 0
    
	## number of original observations
	num.obs		<- sum(!is.na(tmp.hist$weekly_sales))

	##------------------------------------------------------------------
	## basic  projection
	##------------------------------------------------------------------
	if ( (tmp.store > 0) & (tmp.dept %in% c(5, 7, 72, 92)) ) {
		if (num.obs >= minObs) {

            cat("Processing: SD = ", tmp.sdName, "\n")

            ## grab variables to test
            tmp.varlist <- fourierVariable.list[[tmp.sdName]]$coef
            tmp.varlist <- names(tmp.varlist)[ c(grep("_",names(tmp.varlist )), grep("^n",names(tmp.varlist))) ]
            
            ## grab the sales data
            ws <- tmp.hist$ws.min10

            ##------------------------------------------------------------------
            ## [***][d01] -
            ##------------------------------------------------------------------
            if (tmp.dept == 1) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","va_m00","ea_m01","ea_m00","ha_m01","ha_m00","xm_m02","xm_m01")]
                
            ##------------------------------------------------------------------
            ## [***][d02] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 2) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                
            ##------------------------------------------------------------------
            ## [***][d03] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 3) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]

            ##------------------------------------------------------------------
            ## [***][d04] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 4) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                
            ##------------------------------------------------------------------
            ## [***][d05]
            ##------------------------------------------------------------------
            } else if (tmp.dept == 5) {
                #tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("sb_m01","sb_m00","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                tmp.idx  <- grep("_", tmp.varlist)
                if (tmp.store == 28) {
                    tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c(c("sb_m01","sb_m00","fj_m00","td_m00","xm_m01"), tmp.varlist[tmp.idx])]
                } else {
                    tmp.ereg <- tmp.dat[ (tmp.tr_fl == 1) , tmp.varlist[-tmp.idx]]
                    tmp.hhol <- cbind(holiday.df[ (tmp.tr_fl == 1), c(c("sb_m01","sb_m00","fj_m00","td_m00","xm_m01"), tmp.varlist[tmp.idx])], tmp.ereg)
                }
                
            ##------------------------------------------------------------------
            ## [***][d06] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 6) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("mo_m00","fj_m00","td_m00","xm_m02","xm_m01")]

            ##------------------------------------------------------------------
            ## [***][d07] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 7) {
                #tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00","td_m00","td_p01")]
                tmp.idx  <- grep("_", tmp.varlist)
                if (tmp.store == 28) {
                    tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c(c("sb_m00", "td_m00", "xm_m01"), tmp.varlist[tmp.idx])]
                } else {
                    tmp.ereg <- tmp.dat[ (tmp.tr_fl == 1) , tmp.varlist[-tmp.idx]]
                    tmp.hhol <- cbind(holiday.df[ (tmp.tr_fl == 1), c(c("sb_m00", "td_m00", "xm_m01"), tmp.varlist[tmp.idx])], tmp.ereg)
                }
                
            ##------------------------------------------------------------------
            ## [***][d08] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 8) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]

            ##------------------------------------------------------------------
            ## [***][d09] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 9) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]

            ##------------------------------------------------------------------
            ## [***][d10] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 10) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","td_m01","td_m00","xm_m01")]
                
            ##------------------------------------------------------------------
            ## [***][d11] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 11) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("md_m00","fj_m01","fj_m00","ha_m00","td_m01","td_m00","xm_m01")]
                
            ##------------------------------------------------------------------
            ## [***][d12] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 12) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]

            ##------------------------------------------------------------------
            ## [***][d13] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 13) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]

            ##------------------------------------------------------------------
            ## [***][d14] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 14) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","va_m00","td_m01","td_m00","xm_m01")]
                
            ##------------------------------------------------------------------
            ## [***][d16] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 16) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                
            ##------------------------------------------------------------------
            ## [***][d17] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 17) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                
            ##------------------------------------------------------------------
            ## [***][d18] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 18) {
                tmp.ereg <- tmp.dat[ (tmp.tr_fl == 1) , c("ndtemp","ndfuel","nmd1","nmd2","nmd3","nmd4","nmd5")]
                tmp.hhol <- cbind(holiday.df[ (tmp.tr_fl == 1), c("ea_m02","ea_m01","ea_m00")], tmp.ereg)
                #tmp.idx  <- grep("_", tmp.varlist)
                #if (tmp.store == 28) {
                #    tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c(c("sb_m00", "td_m00", "xm_m01"), tmp.varlist[tmp.idx])]
                #} else {
                #    tmp.ereg <- tmp.dat[ (tmp.tr_fl == 1) , tmp.varlist[-tmp.idx]]
                #    tmp.hhol <- cbind(holiday.df[ (tmp.tr_fl == 1), c(c("sb_m00", "td_m00", "xm_m01"), tmp.varlist[tmp.idx])], tmp.ereg)
                #}
                
            ##------------------------------------------------------------------
            ## [***][d20] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 20) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]

            ##------------------------------------------------------------------
            ## [***][d21] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 21) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00")]

            ##------------------------------------------------------------------
            ## [***][d22] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 22) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00")]

            ##------------------------------------------------------------------
            ## [***][d23] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 23) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                
            ##------------------------------------------------------------------
            ## [***][d24] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 24) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]
                
            ##------------------------------------------------------------------
            ## [***][d25] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 25) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]
                
            ##------------------------------------------------------------------
            ## [***][d26] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 26) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00","td_m00","xm_m01")]

            ##------------------------------------------------------------------
            ## [***][d27] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 27) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","td_m01","td_m00","xm_m01")]

            ##------------------------------------------------------------------
            ## [***][d28] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 28) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                
            ##------------------------------------------------------------------
            ## [***][d30] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 30) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","va_m00","ea_m01","ea_m00","md_m00","ld_m03","ld_m02","ld_m01","xm_m01")]

            ##------------------------------------------------------------------
            ## [***][d31] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 31) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","va_m00","ea_m01","ea_m00","md_m00","ld_m03","ld_m02","ld_m01","xm_m01")]

            ##------------------------------------------------------------------
            ## [***][d32] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 32) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]

            ##------------------------------------------------------------------
            ## [***][d33] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 33) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","ea_m00","md_m00","ld_m03","ld_m02","ld_m01","td_m01","td_m00","xm_m02","xm_m01")]

            ##------------------------------------------------------------------
            ## [***][d34] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 34) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","ea_m00","md_m00","fj_m00")]

            ##------------------------------------------------------------------
            ## [***][d38] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 38) {
                tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]

            ##------------------------------------------------------------------
            ## [***][d40] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 40) {
                tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                
            ##------------------------------------------------------------------
            ## [***][d46] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 46) {
                tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]

            ##------------------------------------------------------------------
            ## [***][d55] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 55) {
                tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m02","xm_m01")]

            ##------------------------------------------------------------------
            ## [***][d56] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 56) {
                tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("td_m00","xm_m02","xm_m01")]

            ##------------------------------------------------------------------
            ## [***][d67] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 67) {
                tmp.hhol     <- holiday.df[ (tmp.tr_fl == 1), c("sb_m00","va_m00","ea_m01","ea_m00","ha_m01","ha_m00","xm_m02","xm_m01")]
                
            ##------------------------------------------------------------------
            ## [***][d72] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 72) {
                tmp.idx  <- grep("_", tmp.varlist)
                if (tmp.store == 28) {
                    tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c(c("sb_m00", "td_m00", "xm_m01"), tmp.varlist[tmp.idx])]
                } else {
                    tmp.ereg <- tmp.dat[ (tmp.tr_fl == 1) , tmp.varlist[-tmp.idx]]
                    tmp.hhol <- cbind(holiday.df[ (tmp.tr_fl == 1), c(c("sb_m00", "td_m00", "xm_m01"), tmp.varlist[tmp.idx])], tmp.ereg)
                }
                #tmp.ereg <- tmp.dat[ (tmp.tr_fl == 1) , c("ndtemp","ndfuel","nmd1","nmd2","nmd3","nmd4","nmd5")]
                #tmp.hhol <- cbind(holiday.df[ (tmp.tr_fl == 1), c("sb_m00", "td_m00", "xm_m01")], tmp.ereg)
                #tmp.hhol <- cbind(holiday.df[ (tmp.tr_fl == 1), c("sb_m00", "td_m00", "xm_m01")], tmp.ereg)
                
            ##------------------------------------------------------------------
            ## [***][d79] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 79) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("xm_m01")]

            ##------------------------------------------------------------------
            ## [***][d82] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 82) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("ea_m00","fj_m02","fj_m01","ld_m02","ld_m01")]
                
            ##------------------------------------------------------------------
            ## [***][d87] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 87) {
                tmp.hhol <- NULL
                
            ##------------------------------------------------------------------
            ## [***][d90] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 90) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("ea_m01","td_m01","td_m00","xm_m02","xm_m01","xm_m00")]
                
            ##------------------------------------------------------------------
            ## [***][d91] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 91) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]

            ##------------------------------------------------------------------
            ## [***][d92] - (similar to d90)
            ##------------------------------------------------------------------
            } else if (tmp.dept == 92) {
                #tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m02","xm_m01","xm_m00")]
                tmp.idx  <- grep("_", tmp.varlist)
                if (tmp.store == 28) {
                    tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c(c("td_m01","td_m00","xm_m02","xm_m01","xm_m00"), tmp.varlist[tmp.idx])]
                } else {
                    tmp.ereg <- tmp.dat[ (tmp.tr_fl == 1) , tmp.varlist[-tmp.idx]]
                    tmp.hhol <- cbind(holiday.df[ (tmp.tr_fl == 1), c(c("td_m01","td_m00","xm_m02","xm_m01","xm_m00"), tmp.varlist[tmp.idx])], tmp.ereg)
                }
                
            ##------------------------------------------------------------------
            ## [***][d93] - (similar to d93)
            ##------------------------------------------------------------------
            } else if (tmp.dept == 93) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("td_m01","td_m00","xm_m01")]
                
            ##------------------------------------------------------------------
            ## [***][d94] - (similar to d94)
            ##------------------------------------------------------------------
            } else if (tmp.dept == 94) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("va_m00","mo_m00")]
                
            ##------------------------------------------------------------------
            ## [***][d95] -
            ##------------------------------------------------------------------
            } else if (tmp.dept == 95) {
                tmp.hhol <- holiday.df[ (tmp.tr_fl == 1), c("fj_m01","fj_m00","xm_m01","xm_m00")]
                
            ##------------------------------------------------------------------
            ## [catch-all]
            ##------------------------------------------------------------------
            } else {
                tmp.hhol <- NULL
            }
            
            tmp.fit <- calcFourierOrder(ws, id=tmp.sdName, regs.hist=tmp.hhol, min.order=minOrder, max.order=maxOrder, min.boxcox=0, max.boxcox=1)
            
		} else {

            tmp.fit <- NULL
			
		}
	}
    #return(tmp.fit)
}
names(fourierOrderArima.list) <- paste("SD_",uniqTestSd,sep="")

##------------------------------------------------------------------
## Load the results into a compact forms
##------------------------------------------------------------------
list.names  <- unlist(lapply(fourierOrderArima.list, function(x){x$id}))
list.k      <- unlist(lapply(fourierOrderArima.list, function(x){x$k}))

## matrix of k(s)
cnt <- 1
aic.mat <- matrix(, nrow=length(list.names), ncol=(maxOrder-minOrder+1))
for (i in 1:length(fourierOrderArima.list)) {
    if( !is.null(fourierOrderArima.list[[i]]) ) {
        aic.mat[cnt,] <- fourierOrderArima.list[[i]]$res[,2]
        cnt <- cnt + 1
    }
}


##------------------------------------------------------------------
## Save image
##------------------------------------------------------------------
save(list.names, list.k, aic.mat, fourierOrderArima.list, file="040.003_ArimaOrderSearch_Dept05.07.72.92_Experiment.Rdata")


##------------------------------------------------------------------
## Close connection
##------------------------------------------------------------------
#sink()


