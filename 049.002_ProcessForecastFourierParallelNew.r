##------------------------------------------------------------------
## The purpose of this script is to:
##	1.  Invariably something causes an interruption to the fourier
##      fitting routine, so this script will consolidate indiviudal
##      fit runs.
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
load("005_walmartCombinedData_20140403.Rdata")

##------------------------------------------------------------------
## Read files
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/walmart/data/current/Forecast_Fourier_ArimaOrder_30SigmaTval_D72.mh.k26_D05.mh.k26/")
file.list <- dir()

##------------------------------------------------------------------
## Load in each of the partial fourier files
##------------------------------------------------------------------
vanilla.list    <- list()

for (i in 1:length(file.list)) {
    
    ## load file contents
    tmp.filename    <- file.list[i]
    tmp.sdName      <- unlist(strsplit(tmp.filename,"\\."))[3]
    load(tmp.filename)
    
    ## assign file contents to a list
    vanilla.list[[tmp.sdName]] <- new.fit

}

##------------------------------------------------------------------
## Save image
##------------------------------------------------------------------

## save the results
save(vanilla.list, file="049.002_Forecast_Fourier_ArimaOrder_30SigmaTval_D72.mh.k26_D05.mh.k26.Rdata")



