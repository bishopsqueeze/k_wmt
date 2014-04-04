##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Explode each store/dept combination to be of uniform length
##     (i.e., all have the consistent dates).  Fill the gaps with
##     known values, where possible.
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------

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
load("002_walmartCombinedData.Rdata")

##------------------------------------------------------------------
## Source Utilities
##------------------------------------------------------------------
source("/Users/alexstephens/Development/kaggle/walmart/k_wmt/000_UtilityFunctions.r")

##------------------------------------------------------------------
## Constants
##------------------------------------------------------------------
uniqStores	<- uniq.list$uniqStores
uniqDept	<- uniq.list$uniqDept
uniqSd		<- uniq.list$uniqSd
numStores	<- uniq.list$numStores
numDept		<- uniq.list$numDept
numSd		<- uniq.list$numSd

numWeek		<- 52
startTime	<- 5
maxLength	<- 182		## maximum length of any store/dept dataset


##------------------------------------------------------------------
## Step 1:  Create a prototype dataframe based on a known good
## complete history.  We will use the known good history to build
## a similarly-sized dataframe for store/dept combinations with
## an incomplete time series.
##------------------------------------------------------------------

## extract the size of the known good (01_01)
prototype.df 		<- subset(comb, sd_idx == "01_01")

## create a similarly size dataframe filles with NAs
prototype.na		<- as.data.frame( apply(prototype.df, 2, function(x){rep(NA, maxLength)}) )

## extact values that are good regarless of the s/d
prototype.date		<- as.Date(prototype.df$date)
prototype.nweek		<- prototype.df$nweek
prototype.time		<- seq(from=startTime, to=(startTime + maxLength - 1), by=1)
prototype.isholiday	<- prototype.df$isholiday
prototype.sb_fl		<- prototype.df$sb_fl
prototype.ld_fl		<- prototype.df$ld_fl
prototype.td_fl		<- prototype.df$td_fl
prototype.cd_fl		<- prototype.df$cd_fl
prototype.tr_fl		<- 1*(prototype.date <= "2012-10-26")


##------------------------------------------------------------------
## Step 2:  Explode the timeseries for each individual s/d.  Take 
## care to fill in known gaps ... but otherwise leave the data alone.
##------------------------------------------------------------------

## placeholder list for exploded s/d timeseries
sd.list	<- list()

## <debug> using i <- 77 (SD_01_99)
for (i in 1:numSd) {
    
	##--------------------------------------------------------------
	## Grab the subset of data for this s/d
	##--------------------------------------------------------------
	tmp.sd		<- as.character(droplevels(uniqSd[i]))
	tmp.sdName	<- paste("SD", tmp.sd, sep="_")
	tmp.dat		<- subset(comb, sd_idx == tmp.sd)

	##--------------------------------------------------------------
	## Isolate s/d-specific variables
	##--------------------------------------------------------------
	tmp.store	<- tmp.dat$store[!is.na(tmp.dat$store)][1]
	tmp.dept	<- tmp.dat$dept[!is.na(tmp.dat$dept)][1]
	tmp.type	<- droplevels(tmp.dat$type[!is.na(tmp.dat$type)][1])
	tmp.size	<- tmp.dat$size[!is.na(tmp.dat$size)][1]
	tmp.len		<- nrow(tmp.dat)
    num.train   <- sum(tmp.dat$t_id == "TR")
    num.test    <- sum(tmp.dat$t_id == "TE")

	##--------------------------------------------------------------
	## The first deptartment of each store has a complete set of 
	## observations.  So grab that data for use in completing the 
	## economic timeseries.
	##--------------------------------------------------------------
	tmp.d01			<- subset(comb, (store == tmp.store) & (dept == 1))
	prototype.temp	<- tmp.d01$temperature
	prototype.fuel	<- tmp.d01$fuel_price
	prototype.cpi	<- tmp.d01$cpi
	prototype.unemp	<- tmp.d01$unemployment
	
	##--------------------------------------------------------------
	## explode the data for those stores with an incomplete # of obs.
	##--------------------------------------------------------------
	
	## complete histories
	if ( nrow(tmp.dat) == maxLength ) {
	
		tmp.dat$time	<- prototype.time
		tmp.proto		<- tmp.dat
		tmp.proto$tr_fl	<- 1*(as.Date(prototype.date) <= as.Date("2012-10-26"))
		tmp.expl_fl		<- FALSE 
		
	## incomplete histories
	} else {
	
		## grab the positions of the good data
		date.idx	<- which(prototype.date %in% as.Date(tmp.dat$date))
		tmp.proto	<- prototype.na
		tmp.names	<- names(tmp.dat)
		
		##----------------------------------------------------------
		## Loop over each name in the prototype df.  For each name, load
		## whatever data is available into the proper rows of the prototype df.
		##----------------------------------------------------------
		for (j in 1:length(tmp.names)) {
		
			tmp.class <- class(tmp.dat[,j])
			
			if (tmp.class == "factor") {
				tmp.proto[date.idx,j] <- as.character(tmp.dat[,j])
				tmp.proto[,j] <- as.factor(tmp.proto[,j])
			} else if (tmp.class == "integer") {
				tmp.proto[date.idx,j] <- as.integer(tmp.dat[,j])
			} else if (tmp.class == "numeric") { 		
				tmp.proto[date.idx,j] <- as.numeric(tmp.dat[,j])
			} else if (tmp.class == "character") { 
				tmp.proto[date.idx,j] <- as.character(tmp.dat[,j])
			} else if (tmp.class == "logical") {
				tmp.proto[date.idx,j] <- as.logical(tmp.dat[,j])
			} else {
				##
			}
		}
		
		##--------------------------------------------------------------
        ## fill in NA(s) using known values (where applicable)
		##--------------------------------------------------------------
        tmp.proto$store 		<- tmp.store
		tmp.proto$dept			<- tmp.dept
		tmp.proto$date	 		<- prototype.date
		tmp.proto$tr_fl			<- prototype.tr_fl
		tmp.proto$type			<- tmp.type
		tmp.proto$size			<- tmp.size
		tmp.proto$isholiday		<- prototype.isholiday
		tmp.proto$storeDateKey	<- paste(tmp.store, prototype.date, sep="_")
		tmp.proto$sd_idx		<- as.factor(tmp.sd)
		tmp.proto$nweek			<- prototype.nweek
		tmp.proto$time			<- prototype.time
		tmp.proto$temperature	<- prototype.temp
		tmp.proto$fuel_price	<- prototype.fuel
		tmp.proto$cpi			<- prototype.cpi
		tmp.proto$unemployment	<- prototype.unemp
		tmp.proto$tr_fl			<- 1*(as.Date(prototype.date) <= as.Date("2012-10-26"))
		tmp.expl_fl				<- TRUE 
	}

    ##--------------------------------------------------------------
    ## Extend the CPI and Unemployment rate
    ##--------------------------------------------------------------
    tmp.proto$unemp[is.na(tmp.proto$unemp)] <- tmp.proto$unemp[max(which(!is.na(tmp.proto$unemp)))]
    tmp.proto$cpi[is.na(tmp.proto$cpi)]     <- tmp.proto$cpi[max(which(!is.na(tmp.proto$cpi)))]
    
    ##--------------------------------------------------------------
    ## normalize (mean=0, sd=1) historical data for that s/d
    ##--------------------------------------------------------------
    tmp.proto$ntemp         <- scale(tmp.proto$temperature)
    tmp.proto$ndtemp        <- scale(c(0,diff(tmp.proto$temperature)))
    tmp.proto$nfuel         <- scale(tmp.proto$fuel_price)
    tmp.proto$ndfuel        <- scale(c(0,diff(tmp.proto$fuel_price)))
    tmp.proto$ncpi          <- scale(tmp.proto$cpi)
    #tmp.proto$ndcpi         <- scaleData(c(0,diff(tmp.proto$cpi)))     ## drop b/c interpolated
    tmp.proto$nunemp        <- scale(tmp.proto$unemp)
    #tmp.proto$ndunemp       <- scaleData(c(0,diff(tmp.proto$unemp)))   ## drop b/c slowly varying
    
    ##--------------------------------------------------------------
    ## back-fill nweek data
    ##--------------------------------------------------------------
    ## markdown 1
    if (sum(!is.na(tmp.proto$markdown1)) > 0) {
        bad.md1                 <- which(is.na(tmp.proto$markdown1))
        bad.nweek               <- tmp.proto$nweek[bad.md1]
        tmp.md1                 <- aggregate(markdown1 ~ nweek, data=tmp.proto, FUN=mean)
        tmp.proto$nmd1          <- tmp.proto$markdown1
        tmp.proto$nmd1[bad.md1] <- tmp.md1[bad.nweek,2]
    } else {
        tmp.proto$nmd1          <- tmp.proto$markdown1
    }

    ## markdown 1
    if (sum(!is.na(tmp.proto$markdown2)) > 0) {
        bad.md2         <- which(is.na(tmp.proto$markdown2))
        bad.nweek       <- tmp.proto$nweek[bad.md2]
        tmp.md2         <- aggregate(markdown2 ~ nweek, data=tmp.proto, FUN=mean)
        tmp.proto$nmd2           <- tmp.proto$markdown2
        tmp.proto$nmd2[bad.md2]  <- tmp.md2[bad.nweek,2]
    } else {
        tmp.proto$nmd2          <- tmp.proto$markdown2
    }

    ## markdown 1
    if (sum(!is.na(tmp.proto$markdown3)) > 0) {
        bad.md3         <- which(is.na(tmp.proto$markdown3))
        bad.nweek       <- tmp.proto$nweek[bad.md3]
        tmp.md3         <- aggregate(markdown3 ~ nweek, data=tmp.proto, FUN=mean)
        tmp.proto$nmd3           <- tmp.proto$markdown3
        tmp.proto$nmd3[bad.md3]  <- tmp.md3[bad.nweek,2]
    } else {
        tmp.proto$nmd3          <- tmp.proto$markdown3
    }
 
    ## markdown 4
    if (sum(!is.na(tmp.proto$markdown4)) > 0) {
        bad.md4         <- which(is.na(tmp.proto$markdown4))
        bad.nweek       <- tmp.proto$nweek[bad.md4]
        tmp.md4         <- aggregate(markdown4 ~ nweek, data=tmp.proto, FUN=mean)
        tmp.proto$nmd4           <- tmp.proto$markdown4
        tmp.proto$nmd4[bad.md4]  <- tmp.md4[bad.nweek,2]
    } else {
        tmp.proto$nmd4          <- tmp.proto$markdown4
    }

    ## markdown 5
    if (sum(!is.na(tmp.proto$markdown5)) > 0) {
        bad.md5         <- which(is.na(tmp.proto$markdown5))
        bad.nweek       <- tmp.proto$nweek[bad.md5]
        tmp.md5         <- aggregate(markdown5 ~ nweek, data=tmp.proto, FUN=mean)
        tmp.proto$nmd5           <- tmp.proto$markdown5
        tmp.proto$nmd5[bad.md5]  <- tmp.md5[bad.nweek,2]
    } else {
        tmp.proto$nmd5          <- tmp.proto$markdown5
    }

    ##--------------------------------------------------------------
    ## zero-fill and scale markdown data [???]
    ##--------------------------------------------------------------
    
    ## zero-fill
    tmp.proto$nmd1          <- ifelse(is.na(tmp.proto$nmd1), 0, tmp.proto$nmd1)
    tmp.proto$nmd2          <- ifelse(is.na(tmp.proto$nmd2), 0, tmp.proto$nmd2)
    tmp.proto$nmd3          <- ifelse(is.na(tmp.proto$nmd3), 0, tmp.proto$nmd3)
    tmp.proto$nmd4          <- ifelse(is.na(tmp.proto$nmd4), 0, tmp.proto$nmd4)
    tmp.proto$nmd5          <- ifelse(is.na(tmp.proto$nmd5), 0, tmp.proto$nmd5)
    
    ## get the maximum value of all markdowns for normalization
    maxmd   <- max( tmp.proto[ ,c("nmd1","nmd2","nmd3","nmd4","nmd5")], na.rm=TRUE)
    
    ## scale by the max markdown so we can have relative comparison; also we avoid negative shocks from a markdown
    if ( sum(tmp.proto$nmd1 != 0) > 2 ) {
        tmp.proto$nmd1      <- (tmp.proto$nmd1 / maxmd)
    }
    if ( sum(tmp.proto$nmd2 != 0) > 2 ) {
        tmp.proto$nmd2      <- (tmp.proto$nmd2 / maxmd)
    }
    if ( sum(tmp.proto$nmd3 != 0) > 2 ) {
        tmp.proto$nmd3      <- (tmp.proto$nmd3 / maxmd)
    }
    if ( sum(tmp.proto$nmd4 != 0) > 2 ) {
        tmp.proto$nmd4      <- (tmp.proto$nmd4 / maxmd)
    }
    if ( sum(tmp.proto$nmd5 != 0) > 2 ) {
        tmp.proto$nmd5      <- (tmp.proto$nmd5 / maxmd)
    }
    
	##--------------------------------------------------------------
    ## drop global columns, or columns that are now redundant
    ##--------------------------------------------------------------
    
    ## global vaiables
	tmp.proto$type			<- NULL
	tmp.proto$size			<- NULL
	tmp.proto$storeDateKey	<- NULL

    ## redundant variables
    tmp.proto$temperature   <- NULL
    tmp.proto$fuel_price    <- NULL
    tmp.proto$cpi           <- NULL
    tmp.proto$unemployment  <- NULL
    tmp.proto$unemp         <- NULL
    tmp.proto$markdown1     <- NULL
    tmp.proto$markdown2     <- NULL
    tmp.proto$markdown3     <- NULL
    tmp.proto$markdown4     <- NULL
    tmp.proto$markdown5     <- NULL

	##--------------------------------------------------------------
    ## save results to a list
	##--------------------------------------------------------------
    sd.list[[tmp.sdName]]	<- list(
								store=tmp.store,
								dept=tmp.dept,
								type=tmp.type,
								size=tmp.size,
								sd_idx=tmp.sd,
                                ntrain=num.train,
                                ntest=num.test,
								orig.len=tmp.len,
								expl_fl=tmp.expl_fl,
								data=tmp.proto,
                                mean.sales=mean(tmp.proto$weekly_sales, na.rm=TRUE))
														
if ((i %% 100) == 0) { cat("iteration %d", i, "\n") }
}

##------------------------------------------------------------------
## Step 3:  Save the list to a file
##------------------------------------------------------------------
save(comb, train, test, stores, features, uniq.list, sd.list, prototype.date, file="003_walmartCombinedData.Rdata")

