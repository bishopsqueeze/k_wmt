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
load("005_walmartCombinedData_20140314.Rdata")

##------------------------------------------------------------------
## Load in each of the partial fourier files
##------------------------------------------------------------------
load("./VariableSearch/040.01_FourierVariableSearch_0101_2514_20140328.Rdata")
list.0101_2514 <- fourierVariable.list
rm(fourierVariable.list)

load("./VariableSearch/040.01_FourierVariableSearch_2515_2598_20140328.Rdata")
list.2515_2598 <- fourierVariable.list
rm(fourierVariable.list)

load("./VariableSearch/040.01_FourierVariableSearch_2601_4598_20140328.Rdata")
list.2601_4598 <- fourierVariable.list
rm(fourierVariable.list)


##------------------------------------------------------------------
## Grab each list and dump the contents into a new list
##------------------------------------------------------------------
tmp.ls			<- ls()
tmp.filelist	<- tmp.ls[ grep("list.", tmp.ls) ]
tmp.orders      <- list()

for (i in 1:length(tmp.filelist)) {
	tmp.list	<- get(tmp.filelist[i])
	tmp.names	<- names(tmp.list)
	
	for (i in 1:length(tmp.names)) {
		tmp.orders[[tmp.names[i]]] <- tmp.list[[tmp.names[i]]]
	}
}

sd.orders	<- names(tmp.orders)
sd.orders	<- gsub("SD_", "", x=sd.orders)


##------------------------------------------------------------------
## Compute a lookup table to avoid fits with small numbers of observations
##------------------------------------------------------------------
tr 				<- train
trStoreChar		<- ifelse(tr$store < 10, paste("0", tr$store,sep=""), tr$store)
trDeptChar		<- ifelse(tr$dept < 10, paste("0", tr$dept,sep=""), tr$dept)
tr$sd_idx		<- as.factor(paste(trStoreChar,"_", trDeptChar,sep=""))
tr.tbl			<- table(tr$sd_idx)

##------------------------------------------------------------------
## Review the contents of the fits
##------------------------------------------------------------------
coef.vec        <- unlist(lapply(tmp.orders, function(x){return(length(x$coef)-1)}))
regr_aic.vec    <- unlist(lapply(tmp.orders, function(x){return(x$res[,2])}))
step_aic.vec    <- unlist(lapply(tmp.orders, function(x){return(x$aic)}))


##------------------------------------------------------------------
## Isolate the elements that were not processed
##------------------------------------------------------------------
fits.todo	<- sort( tr.tbl[ -which(names(tr.tbl) %in% sd.orders) ] )


##------------------------------------------------------------------
## Save image
##------------------------------------------------------------------

## define the final list
fourierVariable.list <- tmp.orders

## save the results
save(fourierVariable.list, file="./VariableSearch/041_FourierVariableSearch_All_20140328.Rdata")



