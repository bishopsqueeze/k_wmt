##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Clean bad patches and pixels from the historical series
##  2. Note:  This step involved a lot of visual review, so an
##     effort should be made to automate some of these tasks
##  3. Note:  Datestamp the cleaning effort so that you do not
##     confuse submissions built using a given cleaning run.
##------------------------------------------------------------------
options(warn=1)

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
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
load("004_walmartCombinedData.Rdata")


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
numWeek		<- 52
minTime		<- 5
maxTime		<- 186
interpLim	<- 30	## max number of missing points for interpolation


##------------------------------------------------------------------
## Define manual intervention classes
##------------------------------------------------------------------

## hyper-seasonal sales patterns
manual_18 <- c( "SD_01_18", "SD_02_18", "SD_03_18", "SD_05_18", "SD_06_18", "SD_07_18", "SD_08_18",
				"SD_09_18", "SD_10_18", "SD_12_18", "SD_13_18", "SD_14_18", "SD_15_18", "SD_16_18",
				"SD_17_18", "SD_19_18", "SD_21_18", "SD_23_18", "SD_26_18", "SD_28_18", "SD_29_18", 
				"SD_30_18", "SD_32_18", "SD_33_18", "SD_36_18", "SD_37_18", "SD_38_18", "SD_40_18",
                "SD_41_18", "SD_42_18", "SD_43_18", "SD_44_18")

manual_56 <- c( "SD_36_56", "SD_42_56", "SD_43_56", "SD_44_56")

## department 54 exhibits a consistent drop-off in sales after pixel 60
manual_54 <- c( "SD_01_54", "SD_02_54", "SD_03_54", "SD_04_54", "SD_05_54", "SD_06_54", "SD_07_54",
                "SD_08_54", "SD_09_54", "SD_10_54", "SD_11_54", "SD_12_54", "SD_13_54", "SD_14_54",
                "SD_15_54", "SD_16_54", "SD_17_54", "SD_18_54", "SD_19_54", "SD_20_54", "SD_21_54",
                "SD_22_54", "SD_23_54", "SD_24_54", "SD_25_54", "SD_26_54", "SD_27_54", "SD_28_54",
                "SD_29_54", "SD_31_54", "SD_32_54", "SD_34_54", "SD_35_54", "SD_39_54", "SD_40_54",
                "SD_41_54", "SD_45_54")

## limited data
manual_26 <- c("SD_44_26")

manual_48 <- c("SD_16_48", "SD_26_48")

manual_77 <- c("SD_02_77")

manual_94 <- c( "SD_03_94", "SD_05_94", "SD_07_94", "SD_09_94", "SD_10_94", "SD_12_94", "SD_15_94",
                "SD_16_94", "SD_17_94", "SD_21_94", "SD_23_94", "SD_29_94", "SD_35_94")

manual_96 <- c("SD_17_96")

manual_99 <- c( "SD_01_99", "SD_02_99", "SD_04_99", "SD_06_99", "SD_08_99", "SD_11_99", "SD_13_99",
                "SD_14_99", "SD_19_99", "SD_20_99", "SD_24_99", "SD_26_99", "SD_27_99", "SD_28_99",
                "SD_31_99", "SD_32_99", "SD_34_99", "SD_39_99", "SD_40_99", "SD_41_99")

## varied
manual_58 <- c("SD_05_58", "SD_07_58", "SD_15_58", "SD_16_58", "SD_21_58", "SD_25_58", "SD_29_58", "SD_40_58")


## force to missings to zero for a subset of series
manual_zero <- c(   "SD_36_09",
                    "SD_07_19", "SD_17_19", "SD_20_19",
                    "SD_37_32",
                    "SD_38_56",
                    "SD_43_59",
                    "SD_10_98")

##------------------------------------------------------------------
## Clean the weekly_sales time-series
##------------------------------------------------------------------
clean.list	<- list()
miss.check 	<- 0*vector("numeric", length=numSd)

for (i in 1:numSd) {

	##--------------------------------------------------------------
	## grab s/d locators
	##--------------------------------------------------------------
	tmp.sd		<- as.character(droplevels(uniqSd[i]))
	tmp.sdName	<- paste("SD", tmp.sd, sep="_")
		
	##--------------------------------------------------------------
	## pull the s/d dataset from sd.list()
	##--------------------------------------------------------------
	tmp.list	<- sd.list[[tmp.sdName]]
	tmp.store	<- tmp.list$store
	tmp.dept	<- tmp.list$dept
	
	##--------------------------------------------------------------
	## isolate train/test data
	##--------------------------------------------------------------
	
	## train
	tr.idx		<- which( tmp.list[["data"]]$tr_fl == 1 )
	tr.dat		<- tmp.list[["data"]][ tr.idx, ]
	
	## test
	te.idx		<- which( tmp.list[["data"]]$tr_fl == 0 )	
	te.dat		<- tmp.list[["data"]][ te.idx, ]
	
    ##------------------------------------------------------------------
    ## Define filenames for the figures
    ##------------------------------------------------------------------
	tmp.folder		<- paste(wd, "/Clean/", sep="")
	tmp.filename	<- paste(tmp.folder, tmp.sdName, ".CleanResults.pdf", sep="")
	#dir.create(tmp.folder, showWarnings = FALSE)

	##**************************************************************
	## cleansing rules
	##**************************************************************

    ##**************************************************************
    ## Step 1:  Make bulk replacements of early period mismatches
    ##          or other anomalies identified during a visual
    ##          review of the data.  Otherwise just copy the data.
    ##**************************************************************
    manual.mod  <- TRUE

    if (tmp.sdName == "SD_33_03") {
        mask    <- 1:16
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_36_03") {
        mask    <- 1:14
        tr.cl   <- maskClean(mask, tr.dat)
    ## very limited data
    } else if (tmp.sdName == "SD_43_05") {
        mask    <- 1:82
        tr.cl   <- maskClean(mask, tr.dat)
    ## offset
    } else if (tmp.sdName == "SD_14_08") {
        offset  <- median(tr.dat$weekly_sales[124:143], na.rm=TRUE) - median(tr.dat$weekly_sales[1:123], na.rm=TRUE)
        tr.cl   <- c( tr.dat$weekly_sales[1:123]-offset, tr.dat$weekly_sales[124:143] )
    ## anti-step
    } else if (tmp.sdName == "SD_35_08") {
        mask    <- 1:30
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_36_10") {
        mask    <- 1:14
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_33_11") {
        mask    <- 1:14
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_33_12") {
        mask    <- 1:15
        tr.cl   <- maskClean(mask, tr.dat)
    ## anti-step
    } else if (tmp.sdName == "SD_43_13") {
        mask    <- 1:40
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_11_19") {
        mask    <- 1:75
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_14_19") {
        mask    <- 1:83
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_22_19") {
        mask    <- 1:83
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_27_19") {
        mask    <- 1:82
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_41_19") {
        mask    <- 1:78
        tr.cl   <- maskClean(mask, tr.dat)
    ## anti-step (discontinued line?)
    } else if (tmp.sdName == "SD_43_28") {
        mask    <- 1:80
        tr.cl   <- maskClean(mask, tr.dat)
    ## offset
    } else if (tmp.sdName == "SD_14_38") {
        offset  <- median(tr.dat$weekly_sales[122:143], na.rm=TRUE) - median(tr.dat$weekly_sales[1:121], na.rm=TRUE)
        tr.cl   <- c( tr.dat$weekly_sales[1:121]+offset, tr.dat$weekly_sales[122:143] )
    ## offset
    } else if (tmp.sdName == "SD_14_40") {
        offset  <- median(tr.dat$weekly_sales[124:143], na.rm=TRUE) - median(tr.dat$weekly_sales[1:123], na.rm=TRUE)
        tr.cl   <- c( tr.dat$weekly_sales[1:123]+offset, tr.dat$weekly_sales[124:143] )
    } else if (tmp.sdName == "SD_35_40") {
        mask    <- 1:30
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_07_48") {
        mask    <- 1:80
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_09_48") {
        mask    <- 1:80
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_41_48") {
        mask    <- 1:60
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_18_50") {
        mask    <- 1:72
        tr.cl   <- maskClean(mask, tr.dat)
    ## drop-off
    } else if (tmp.sdName == "SD_07_59") {
        mask    <- 1:66
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_02_60") {
        mask    <- 1:60
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_06_60") {
        mask    <- 1:60
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_09_60") {
        mask    <- 1:60
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_11_60") {
        mask    <- 1:60
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_15_60") {
        mask    <- 1:60
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_16_60") {
        mask    <- 1:60
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_18_60") {
        mask    <- 1:60
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_23_60") {
        mask    <- 1:60
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_24_60") {
        mask    <- 1:60
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_25_60") {
        mask    <- 1:60
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_26_60") {
        mask    <- 1:60
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_27_60") {
        mask    <- 1:40
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_29_60") {
        mask    <- 1:40
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_34_60") {
        mask    <- 1:40
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_39_60") {
        mask    <- 1:60
        tr.cl   <- maskClean(mask, tr.dat)
    ## offset
    } else if (tmp.sdName == "SD_30_74") {
        offset  <- median(tr.dat$weekly_sales[86:143], na.rm=TRUE) - median(tr.dat$weekly_sales[1:85], na.rm=TRUE)
        tr.cl   <- c( tr.dat$weekly_sales[1:85]+offset, tr.dat$weekly_sales[86:143] )
    } else if (tmp.sdName == "SD_42_74") {
        mask    <- 1:36
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_10_79") {
        mask    <- 1:40
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_35_79") {
        mask    <- 1:40
        tr.cl   <- maskClean(mask, tr.dat)
    ## outlier (no markdown or holiday)
    } else if (tmp.sdName == "SD_43_79") {
        mask    <- 79:79
        tr.cl   <- maskClean(mask, tr.dat)
    ## spike down
    } else if (tmp.sdName == "SD_01_80") {
        mask    <- 76:78
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_07_80") {
        mask    <- 1:33
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_16_80") {
        mask    <- 1:86
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_18_80") {
        mask    <- 1:87
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_23_80") {
        mask    <- 1:60
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_25_80") {
        mask    <- 1:60
        tr.cl   <- maskClean(mask, tr.dat)
    ## spike down
    } else if (tmp.sdName == "SD_30_80") {
        mask    <- 79:86
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_45_80") {
        mask    <- 1:60
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_07_83") {
        mask    <- 1:45
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_16_83") {
        mask    <- 1:87
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_18_83") {
        mask    <- 1:79
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_23_83") {
        mask    <- 1:40
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_33_83") {
        mask    <- 1:40
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_28_85") {
        mask    <- 1:12
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_44_85") {
        mask    <- 1:28
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_36_87") {
        mask    <- 1:26
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_38_87") {
        mask    <- 1:34
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_18_90") {
        mask    <- 1:87
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_23_90") {
        mask    <- 1:40
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_05_91") {
        mask    <- 1:10
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_07_91") {
        mask    <- 1:26
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_10_91") {
        mask    <- 1:1
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_14_91") {
        mask    <- 1:1
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_16_91") {
        mask    <- 1:89
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_18_91") {
        mask    <- 1:87
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_20_91") {
        mask    <- 1:1
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_23_91") {
        mask    <- 1:40
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_25_91") {
        mask    <- 1:3
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_35_91") {
        mask    <- 1:30
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_45_91") {
        mask    <- 1:1
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_05_92") {
        mask    <- 1:10
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_14_92") {
        mask    <- 1:1
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_18_92") {
        mask    <- 1:87
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_45_92") {
        mask    <- 1:1
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_14_93") {
        mask    <- 1:1
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_16_93") {
        mask    <- 1:87
        tr.cl   <- maskClean(mask, tr.dat)
    ## offset
    } else if (tmp.sdName == "SD_18_93") {
        mask    <- 1:88
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_23_93") {
        mask    <- 1:40
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_18_94") {
        mask    <- 1:87
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_25_94") {
        mask    <- 1:79
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_45_94") {
        mask    <- 1:30
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_01_96") {
        mask    <- 1:22
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_05_96") {
        mask    <- 1:6
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_03_97") {
        mask    <- 1:62
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_14_97") {
        mask    <- 1:1
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_18_97") {
        mask    <- 1:88
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_23_97") {
        mask    <- 1:40
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_18_98") {
        mask    <- 1:88
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_25_98") {
        mask    <- 1:84
        tr.cl   <- maskClean(mask, tr.dat)
    } else if (tmp.sdName == "SD_45_98") {
        mask    <- 1:56
        tr.cl   <- maskClean(mask, tr.dat)
    ## bulk replace all of the manual_54
    } else if (tmp.sdName %in% manual_54) {
        mask    <- 1:60
        tr.cl   <- maskClean(mask, tr.dat)
    }else {
        tr.cl       <- tr.dat$weekly_sales
        manual.mod  <- FALSE
    }

    # define a new weekly_sales vector
    tr.dat$ws.man	<- tr.cl
    te.dat$ws.man	<- te.dat$weekly_sales
    
    ## load the new vector into the dataframe
    tmp.list[["data"]]$ws.man   <- c(tr.dat$ws.man,  te.dat$ws.man)

    ##--------------------------------------------------------------
    ## Identify missings in the first pass cleaned data
    ##--------------------------------------------------------------
    bad.idx			<- is.na(tr.dat$ws.man)
    bad.sum			<- sum(bad.idx)
    mean.sales		<- mean(tr.dat$ws.man, na.rm=TRUE)
    sd.sales		<- sd(tr.dat$ws.man, na.rm=TRUE)


    ##**************************************************************
    ## Step 2:  Based on the number of missings or the magnitude of
    ##          sales within a given data frame, perform an
    ##          additional set of cleaning steps
    ##**************************************************************

	##--------------------------------------------------------------
	## no cleansing if no missings
	##--------------------------------------------------------------
	if ( bad.sum == 0 ) {
	
		## copy the original
		tmp.list[["data"]]$ws.fin	<- tmp.list[["data"]]$ws.man
		clean.type	<- c("none")
	
	##--------------------------------------------------------------
	## no cleansing if not in the test dataset
	##--------------------------------------------------------------
	} else if ( length(which(uniqTestSd %in% tmp.sd)) == 0 ) {
	
		## copy the original
		tmp.list[["data"]]$ws.fin	<- tmp.list[["data"]]$ws.man
		clean.type	<- c("none")
		
	##--------------------------------------------------------------
	## if all bad, just force to zero
	##--------------------------------------------------------------
	} else if ( bad.sum == 143 ) {
	
		## copy the original, then force to 0
		tmp.list[["data"]]$ws.fin	<- tmp.list[["data"]]$ws.man
		tmp.list[["data"]]$ws.fin   <- 0
		clean.type	<- c("zero")
		
	##--------------------------------------------------------------
	## linear interpolation for limited missings
	##--------------------------------------------------------------
	} else if ( bad.sum <= interpLim ) {
	
		x 		<- tr.dat$time[ !bad.idx ]
		y 		<- tr.dat$ws.man[ !bad.idx ]
		xout	<- tr.dat$time
		yout	<- approx(x, y, xout, method="linear", rule=2)$y	## interpolated
	
		tr.dat$ws.fin               <- yout
		te.dat$ws.fin               <- te.dat$weekly_sales
		tmp.list[["data"]]$ws.fin   <- c(tr.dat$ws.fin, te.dat$ws.fin)
		clean.type	<- c("interp")
	
	##--------------------------------------------------------------
	## median replace for small dollar (<$100) historical averages
	##--------------------------------------------------------------
	} else if ( mean.sales < 100 ) {
	
        tr.dat$ws.fin               <- tr.dat$ws.man
  		tr.dat$ws.fin[ bad.idx ]    <- median(tr.dat$ws.man, na.rm=TRUE)
		te.dat$ws.fin               <- te.dat$weekly_sales
		tmp.list[["data"]]$ws.fin	<- c(tr.dat$ws.fin,  te.dat$ws.fin)
		clean.type	<- c("small_dollar_median")
	
	##--------------------------------------------------------------
	## median replace for small numbers of observations (>20)
	##--------------------------------------------------------------
	} else if ( bad.sum >= 123 ) {
        
        tr.dat$ws.fin               <- tr.dat$ws.man
  		tr.dat$ws.fin[ bad.idx ]    <- median(tr.dat$ws.man, na.rm=TRUE)
		te.dat$ws.fin               <- te.dat$weekly_sales
		tmp.list[["data"]]$ws.fin	<- c(tr.dat$ws.fin,  te.dat$ws.fin)
		clean.type	<- c("small_count_median")
        
	##--------------------------------------------------------------
	## remaining items handled based on a visual review of the data
	##--------------------------------------------------------------
	} else {		
			
		##--------------------------------------------------------------
		## Set hyper-seasonal lulls to 0
		##--------------------------------------------------------------
		if (tmp.sdName %in% c(manual_18, manual_56)) {
	
			tr.dat$ws.fin               <- tr.dat$ws.man
			tr.dat$ws.fin[ bad.idx ]	<- 0
			te.dat$ws.fin				<- te.dat$weekly_sales
			tmp.list[["data"]]$ws.fin   <- c(tr.dat$ws.fin, te.dat$ws.fin)
	
			clean.type	<- c("manual_zero") 

        ##--------------------------------------------------------------
        ## Set missings to 0 for a subset of visually reviewed
        ##--------------------------------------------------------------
        } else if (tmp.sdName %in% c(manual_zero)) {
	
            tr.dat$ws.fin               <- tr.dat$ws.man
            tr.dat$ws.fin[ bad.idx ]	<- 0
            te.dat$ws.fin				<- te.dat$weekly_sales
            tmp.list[["data"]]$ws.fin   <- c(tr.dat$ws.fin, te.dat$ws.fin)
    
            clean.type	<- c("manual_zero")

        ##--------------------------------------------------------------
		## interp or median depending on store/dept
		##--------------------------------------------------------------
		} else if (tmp.sdName %in% manual_58) {
		
			if (tmp.store %in% c(5, 15, 21, 25, 29) ) {
			
				x 		<- tr.dat$time[ !bad.idx ]
				y 		<- tr.dat$ws.man[ !bad.idx ]
				xout	<- tr.dat$time
				yout	<- approx(x, y, xout, method="linear", rule=2)$y	## interpolated
	
				tr.dat$ws.fin               <- yout
				te.dat$ws.fin               <- te.dat$weekly_sales
				tmp.list[["data"]]$ws.fin	<- c(tr.dat$ws.fin,  te.dat$ws.fin)
				clean.type	<- c("manual_interp")
				
			} else {

				tr.dat$ws.fin               <- tr.dat$ws.man
				tr.dat$ws.fin[ bad.idx ]	<- median(tr.dat$ws.man, na.rm=TRUE)
				te.dat$ws.fin				<- te.dat$weekly_sales
				tmp.list[["data"]]$ws.fin	<- c(tr.dat$ws.fin,  te.dat$ws.fin)
				clean.type	<- c("manual_median")
			}
			 
		
		##--------------------------------------------------------------
		## mean replace store 77
		##--------------------------------------------------------------
		} else if (tmp.sdName %in% manual_77) {

				tr.dat$ws.fin               <- tr.dat$ws.man
				tr.dat$ws.fin[ bad.idx ]	<- mean(tr.dat$ws.man, na.rm=TRUE)
				te.dat$ws.fin				<- te.dat$ws.man
				tmp.list[["data"]]$ws.fin   <- c(tr.dat$ws.fin,  te.dat$ws.fin)
		
				clean.type	<- c("manual_mean") 
				
		##--------------------------------------------------------------
		## median replace
		##--------------------------------------------------------------
		} else if (tmp.sdName %in% c(manual_26, manual_48, manual_94, manual_96, manual_99)) {

				tr.dat$ws.fin				<- tr.dat$ws.man
				tr.dat$ws.fin[ bad.idx ]	<- median(tr.dat$ws.man, na.rm=TRUE)
				te.dat$ws.fin				<- te.dat$ws.man
				tmp.list[["data"]]$ws.fin   <- c(tr.dat$ws.fin,  te.dat$ws.fin)
		
				clean.type	<- c("manual_median") 
				
		##--------------------------------------------------------------
		## warn on no-hit
		##--------------------------------------------------------------
		} else {
				cat("S/D --> ", tmp.sdName, "\n")
				warning("no match")
				miss.check[i] <- 1	
		}

	}
	
	##--------------------------------------------------------------
	## Define a series of floored sales vectors.  If we do a Box-Cox
    ## transformation, then some of the floors (e.g., 1) might result
    ## in errors.  So, set floors at $0, $1 and $10 ... and then pick
    ## and choose based on the application.
	##--------------------------------------------------------------
	tmp.list[["data"]]$ws.min00	<- pmax(0, tmp.list[["data"]]$ws.fin)
	tmp.list[["data"]]$ws.min01	<- pmax(1, tmp.list[["data"]]$ws.fin)
	tmp.list[["data"]]$ws.min10	<- pmax(10, tmp.list[["data"]]$ws.fin)
	
	##--------------------------------------------------------------
	## Assign the clean type
	##--------------------------------------------------------------
	tmp.list$clean.type	<- clean.type
	tmp.list$manual.mod	<- manual.mod
    
	##--------------------------------------------------------------
	## Copy to a new list
	##--------------------------------------------------------------
	clean.list[[tmp.sdName]] <- tmp.list
	
	##--------------------------------------------------------------
	## Plot before and after (for visual cross-check
	##--------------------------------------------------------------
    ## setup directory/filename
    if ( bad.sum < 143 ) {
        pdf(tmp.filename)
            plot(tmp.list[["data"]]$weekly_sales[1:143], col="black", type="b")
            lines(tmp.list[["data"]]$ws.fin[1:143], col="orange", lwd=2)
        dev.off()
    }
	
## report progress
if ((i %% 100) == 0) { cat("iteration %d", i, "\n") }
}

##------------------------------------------------------------------
## Save results [APPLY THE TIMESTAMP
##------------------------------------------------------------------
#save(	comb, train, test, stores, features,
#		uniq.list, sd.list,
#		prototype.date, time.df, holiday.df,
#		clean.list,
#		file="005_walmartCombinedData_20140314.Rdata")
		







