##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Load date masks
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(timeDate)
library(Hmisc)

#------------------------------------------------------------------
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
load("003_walmartCombinedData.Rdata")

##------------------------------------------------------------------
## Source Utilities
##------------------------------------------------------------------
source("/Users/alexstephens/Development/kaggle/walmart/code/000_UtilityFunctions.r")

##------------------------------------------------------------------
## Constants
##------------------------------------------------------------------
numWeek		<- 52
minTime		<- 5
maxTime		<- 186

##------------------------------------------------------------------
## Create a set of "time" indices for the observation window
##------------------------------------------------------------------

## create (date, nweek, time) variables
time.df <- data.frame(
                date=as.Date(unique(comb$date)),
                nweek=as.numeric(format(as.Date(unique(comb$date), format="%Y-%m-%d"), format="%V")),
                time=seq(from=minTime, to=maxTime, by=1)
                    )

## Add (day, month, year) variables
time.df$year	<- format(time.df$date, "%Y")
time.df$month	<- format(time.df$date, "%m")
time.df$day		<- format(time.df$date, "%d")


##------------------------------------------------------------------
## Create date dummy flag vectors for major holidays (+/- 2 weeks)
##------------------------------------------------------------------

##******************************************************************
## Superbowl ("2010-02-12", "2011-02-11", "2012-02-10", "2013-02-08")
##******************************************************************
d		<- as.Date(c("2010-02-12", "2011-02-11", "2012-02-10", "2013-02-08"))
sb_m01	<- createDateFlags(d, time.df$date, n.lag=-1)
sb_m00	<- createDateFlags(d, time.df$date, n.lag=0)


##******************************************************************
## Valentines ("2010-02-14", "2011-02-14", "2012-02-14", "2013-02-14")
##******************************************************************
d 		<- as.Date(c("2010-02-14", "2011-02-14", "2012-02-14", "2013-02-14"))
va_m02	<- createDateFlags(d, time.df$date, n.lag=-2)
va_m01	<- createDateFlags(d, time.df$date, n.lag=-1)
va_m00	<- createDateFlags(d, time.df$date, n.lag=0)
va_p01	<- createDateFlags(d, time.df$date, n.lag=1)
va_p02	<- createDateFlags(d, time.df$date, n.lag=2)


##******************************************************************
## Easter ("2010-04-04" "2011-04-24" "2012-04-08" "2013-03-31")
##******************************************************************
d		<- as.Date(Easter(2010:2013))
ea_m02	<- createDateFlags(d, time.df$date, n.lag=-2)
ea_m01	<- createDateFlags(d, time.df$date, n.lag=-1)
ea_m00	<- createDateFlags(d, time.df$date, n.lag=0)
ea_p01	<- createDateFlags(d, time.df$date, n.lag=1)
ea_p02	<- createDateFlags(d, time.df$date, n.lag=2)


##******************************************************************
## Mother's Day c("2010-05-31", "2011-05-08", "2012-05-13", "2013-05-12")
##******************************************************************
d 		<- as.Date(c("2010-05-09", "2011-05-08", "2012-05-13", "2013-05-12"))
mo_m01	<- createDateFlags(d, time.df$date, n.lag=-1)
mo_m00	<- createDateFlags(d, time.df$date, n.lag=0)
mo_p01	<- createDateFlags(d, time.df$date, n.lag=+1)


##******************************************************************
## Memorial Day c("2010-05-31", "2011-05-31", "2012-05-31", "2013-05-31")
##******************************************************************
d 		<- as.Date(c("2010-05-31", "2011-05-31", "2012-05-31", "2013-05-31"))
md_m01	<- createDateFlags(d, time.df$date, n.lag=-1)
md_m00	<- createDateFlags(d, time.df$date, n.lag=0)
md_p01	<- createDateFlags(d, time.df$date, n.lag=+1)

##******************************************************************
## 4th of July c("2010-07-04", "2011-07-04", "2012-07-04", "2013-07-04")
##******************************************************************
d 		<- as.Date(c("2010-07-04", "2011-07-04", "2012-07-04", "2013-07-04"))
fj_m02	<- createDateFlags(d, time.df$date, n.lag=-2)
fj_m01	<- createDateFlags(d, time.df$date, n.lag=-1)
fj_m00	<- createDateFlags(d, time.df$date, n.lag=0)
fj_p01	<- createDateFlags(d, time.df$date, n.lag=+1)

##******************************************************************
## Back-to_school c("2010-08-31", "2011-08-31", "2012-08-31", "2013-08-31")
##******************************************************************
d 		<- as.Date(c("2010-08-31", "2011-08-31", "2012-08-31", "2013-08-31"))
bs_m02	<- createDateFlags(d, time.df$date, n.lag=-2)
bs_m01	<- createDateFlags(d, time.df$date, n.lag=-1)
bs_m00	<- createDateFlags(d, time.df$date, n.lag=0)


##******************************************************************
## Labor Day c("2010-09-10", "2011-09-09", "2012-09-07", "2013-09-06")
##******************************************************************
d 		<- as.Date(c("2010-09-10", "2011-09-09", "2012-09-07"))
ld_m01	<- createDateFlags(d, time.df$date, n.lag=-1)
ld_m00	<- createDateFlags(d, time.df$date, n.lag=0)
ld_p01	<- createDateFlags(d, time.df$date, n.lag=1)


##******************************************************************
## Halloween c("2010-10-31", "2011-10-31", "2012-10-31", "2013-10-31")
##******************************************************************
d 		<- as.Date(c("2010-10-31", "2011-10-31", "2012-10-31"))
ha_m01	<- createDateFlags(d, time.df$date, n.lag=-1)
ha_m00	<- createDateFlags(d, time.df$date, n.lag=0)
ha_p01	<- createDateFlags(d, time.df$date, n.lag=1)


##******************************************************************
## Thanksgiving c("2010-11-26", "2011-11-25", "2012-11-23")
##******************************************************************
d 		<- as.Date(c("2010-11-26", "2011-11-25", "2012-11-23"))
td_m01	<- createDateFlags(d, time.df$date, n.lag=-1)
td_m00	<- createDateFlags(d, time.df$date, n.lag=0)
td_p01	<- createDateFlags(d, time.df$date, n.lag=1)


##******************************************************************
## Christmas c("2010-12-25", "2011-12-25", "2012-12-25")
##******************************************************************
d		<- as.Date(c("2010-12-25", "2011-12-25", "2012-12-25"))
xm_m02	<- createDateFlags(d, time.df$date, n.lag=-2)
xm_m01	<- createDateFlags(d, time.df$date, n.lag=-1)
xm_m00	<- createDateFlags(d, time.df$date, n.lag=0)
xm_p01	<- createDateFlags(d, time.df$date, n.lag=1)
xm_p02	<- createDateFlags(d, time.df$date, n.lag=2)


##------------------------------------------------------------------
## Append all holidays to the time.df data.frame
##------------------------------------------------------------------
new.df	<- data.frame(	time.df, 
						sb_m00,
						va_m02, va_m01, va_m00, va_p01, va_p02,
						ea_m02, ea_m01, ea_m00, ea_p01, ea_p02
                        mo_m01, mo_m00, mo_p01,
						md_m01, md_m00, md_p01,
						fj_m01, fj_m00, fj_p01,
						ld_m01, ld_m00, ld_p01,
                        ha_m01, ha_m00, ha_p01,
                        td_m01, td_m00, td_p01,
						xm_m02, xm_m01, xm_m00, xm_p01, xm_p02
						)

##------------------------------------------------------------------
## Clean out NA(s) and call it the holiday df
##------------------------------------------------------------------
new.df[is.na(new.df)]   <- 0
holiday.df              <- new.df

##------------------------------------------------------------------
## Save results
##------------------------------------------------------------------

## Combined version 03/10/2014
save(	comb, train, test, stores, features, 
		uniq.list, sd.list, prototype.date,
		time.df, holiday.df,
		file="004_walmartCombinedData.Rdata")
		
