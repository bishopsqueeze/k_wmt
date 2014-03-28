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
load("./OrderSearch/040_FourierOrderSearch_Only40_0101_1580_20140326.Rdata")
list.0101_1580 <- fourierRegression.list
rm(fourierRegression.list)

load("./OrderSearch/040_FourierOrderSearch_Only40_1580_1598_20140326.Rdata")
list.1580_1598 <- fourierRegression.list
rm(fourierRegression.list)

load("./OrderSearch/040_FourierOrderSearch_Only40_1600_4598_20140326.Rdata")
list.1600_4598 <- fourierRegression.list
rm(fourierRegression.list)


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
orders.list <- tmp.orders

## save the results
save(orders.list, file="./OrderSearch/040_FourierOrderSearch_Only40_All_20140326.Rdata")



