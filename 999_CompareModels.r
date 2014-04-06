##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Take the results of various in-sample fits and compare the
##     raw mean absolute errors.
##  2. It also consolidates all the results into a single file that
##     I use to generate the final forecast.
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(forecast)

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
## Load development data
##------------------------------------------------------------------
load("005_walmartCombinedData_20140326.Rdata")

##------------------------------------------------------------------
## Load model fit data
##------------------------------------------------------------------
load("./Forecasts/010_ForecastMedian_20140314.Rdata")                   ## median.list
load("./Forecasts/011_ForecastMean_20140314.Rdata")                     ## mean.list
load("./Forecasts/020_ForecastStl_20140314.Rdata")                      ## slt.list
load("./Forecasts/030_ForecastTslm_20140314.Rdata")                     ## tslm.list
load("./Forecasts/031_ForecastLm_20140314.Rdata")                       ## lm.list
load("./Forecasts/032_ForecastLmMulti_20140314.Rdata")                  ## lmMulti.list
load("./Forecasts/033_ForecastQrMulti_20140314.Rdata")                  ## qrMulti.list
load("./Forecasts/041_ForecastFourier_All_Min100_20140314.Rdata")       ## fourier.list
#load("041.01_VanillaFourier_All_Min100_20140314.Rdata")    ## vanilla.list (S011)
#load("041.01_VanillaFourier_All_Min100_20140326.Rdata")    ## vanilla.list (S012)
#load("./Forecasts/042_VanillaFourier_All_Min100_20140326.Rdata")        ## vanilla.list (S013)
#load("042_VanillaFourier_TOP10TEST.Rdata")                              ## partical order serach based (S014)
#load("042.001_Forecast_MinK20_S021_04032014.Rdata")                     ## more order saerch (S020)
#load("049.002_Forecast_Fourier_ArimaOrder_20SigmaTval.Rdata")           ## updated order search (S023)
#load("049.002_Forecast_Fourier_ArimaOrder_30SigmaTval.Rdata")           ## updated order search (S024)
#load("049.002_Forecast_Fourier_ArimaOrder_30SigmaTval_Exp72.Rdata")           ## updated d72 only (S025)
#load("049.002_Forecast_Fourier_ArimaOrder_30SigmaTval_D72.mh.k26_D07.mh.kfit.Rdata")           ## same as S025 w/updated d7 (S026)
load("049.002_Forecast_Fourier_ArimaOrder_30SigmaTval_D72.mh.k26_D05.mh.k26.Rdata")           ## same as S025 w/updated d5 (S027)
#load("./Forecasts/050_ForecastNweek_20140314.Rdata")                   ## nweek.list (semi-historical)
load("050.002_RqForecastNweek_20140326.Rdata")                          ## nweek.list (semi-historical)
nweek_trend.list    <- nweek.list; rm(nweek.list)
load("050.003_RqForecastNweekNoTrend_20140326.Rdata")                   ## nweek.list (semi-historical, no-trend)
nweek_notrend.list  <- nweek.list; rm(nweek.list)

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
minObs          <- 50

##------------------------------------------------------------------
## Use <uniqTestSd> to align results data
##------------------------------------------------------------------
res.col                 <- 11
res.matrix              <- matrix(, nrow=numTestSd, ncol=res.col)
rownames(res.matrix)    <- uniqTestSd
colnames(res.matrix)    <- c("median", "mean", "stl", "tslm", "lm", "lmmulti", "qrmulti", "fourier", "vanilla", "nweek", "nweek.notrend")
names.sd                <- paste("SD_",uniqTestSd,sep="")

## median
median.mae  <- do.call(rbind, lapply(median.list, function(x){x$mae}))
median.idx  <- which( names.sd %in% rownames(median.mae))
res.matrix[ median.idx, 1]  <- median.mae

## mean
mean.mae    <- do.call(rbind, lapply(mean.list, function(x){x$mae}))
mean.idx    <- which( names.sd %in% rownames(mean.mae))
res.matrix[ mean.idx, 2]  <- mean.mae

## stl
stl.mae     <- do.call(rbind, lapply(stl.list, function(x){x$mae}))
stl.idx     <- which( names.sd %in% rownames(stl.mae))
res.matrix[ stl.idx, 3]  <- stl.mae

## tslm
tslm.mae     <- do.call(rbind, lapply(tslm.list, function(x){x$mae}))
tslm.idx     <- which( names.sd %in% rownames(tslm.mae))
res.matrix[ tslm.idx, 4]  <- tslm.mae

## vanilla lm
lm.mae     <- do.call(rbind, lapply(lm.list, function(x){x$mae}))
lm.idx     <- which( names.sd %in% rownames(lm.mae))
res.matrix[ lm.idx, 5]  <- lm.mae

## multivariate lm
lmMulti.mae     <- do.call(rbind, lapply(lmMulti.list, function(x){x$mae}))
lmMulti.idx     <- which( names.sd %in% rownames(lmMulti.mae))
res.matrix[ lmMulti.idx, 6]  <- lmMulti.mae

## multivariate qr (quantile regression)
qrMulti.mae     <- do.call(rbind, lapply(qrMulti.list, function(x){x$mae}))
qrMulti.idx     <- which( names.sd %in% rownames(qrMulti.mae))
res.matrix[ lmMulti.idx, 7]  <- qrMulti.mae

## fourier (1st (bad) attempt)
fourier.mae     <- do.call(rbind, lapply(fourier.list, function(x){x$mae}))
fourier.idx     <- which( names.sd %in% rownames(fourier.mae))
res.matrix[ fourier.idx, 8]  <- fourier.mae

## vanilla fourier (2nd attempt)
vanilla.mae     <- do.call(rbind, lapply(vanilla.list, function(x){x$mae}))
vanilla.idx     <- which( names.sd %in% rownames(vanilla.mae))
res.matrix[vanilla.idx, 9]  <- vanilla.mae

## nweek (part copy of history, part regression) -- used for sparse histories
nweek_trend.mae     <- do.call(rbind, lapply(nweek_trend.list, function(x){x$mae}))
nweek_trend.idx     <- which( names.sd %in% rownames(nweek_trend.mae))
res.matrix[ nweek_trend.idx, 10]  <- nweek_trend.mae

## nweek (part copy of history, part regression) -- used for sparse histories
nweek_notrend.mae     <- do.call(rbind, lapply(nweek_notrend.list, function(x){x$mae}))
nweek_notrend.idx     <- which( names.sd %in% rownames(nweek_notrend.mae))
res.matrix[ nweek_notrend.idx, 11]  <- nweek_notrend.mae

##------------------------------------------------------------------
## Save image
##------------------------------------------------------------------
save(   median.list,
        mean.list,
        stl.list,
        tslm.list,
        lm.list,
        lmMulti.list,
        qrMulti.list,
        fourier.list,
        vanilla.list,
        nweek_trend.list, nweek_notrend.list,
        res.matrix,
        file="999_CompareModels_S027_20140405.Rdata")

















