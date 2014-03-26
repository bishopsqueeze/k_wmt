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
load("041.01_VanillaFourier_0101_4327_20140314.Rdata")
list.0101_4327 <- vanilla.list
rm(vanilla.list)

load("041.01_VanillaFourier_4329_4398_20140314.Rdata")
list.4329_4399 <- vanilla.list
rm(vanilla.list)

load("041.01_VanillaFourier_4401_4598_20140314.Rdata")
list.4401_4499 <- vanilla.list
rm(vanilla.list)


##------------------------------------------------------------------
## Grab each list and dump the contents into a new list
##------------------------------------------------------------------
tmp.ls			<- ls()
tmp.filelist	<- tmp.ls[ grep("list.", tmp.ls) ]
tmp.vanilla		<- list()

for (i in 1:length(tmp.filelist)) {
	tmp.list	<- get(tmp.filelist[i])
	tmp.names	<- names(tmp.list)
	
	for (i in 1:length(tmp.names)) {
		tmp.vanilla[[tmp.names[i]]] <- tmp.list[[tmp.names[i]]]
	}
}

sd.vanilla	<- names(tmp.vanilla)
sd.vanilla	<- gsub("SD_", "", x=sd.vanilla)

##------------------------------------------------------------------
## Compute a lookup table to avoid fits with small numbers of observations
##------------------------------------------------------------------
tr 				<- train
trStoreChar		<- ifelse(tr$store < 10, paste("0", tr$store,sep=""), tr$store)
trDeptChar		<- ifelse(tr$dept < 10, paste("0", tr$dept,sep=""), tr$dept)
tr$sd_idx		<- as.factor(paste(trStoreChar,"_", trDeptChar,sep=""))
tr.tbl			<- table(tr$sd_idx)


##------------------------------------------------------------------
## Isolate the elements that were not processed
##------------------------------------------------------------------
fits.todo	<- sort( tr.tbl[ -which(names(tr.tbl) %in% sd.vanilla) ] )


##------------------------------------------------------------------
## Save image
##------------------------------------------------------------------

## define the final list
vanilla.list <- tmp.vanilla

## save the results
save(vanilla.list, file="041.01_VanillaFourier_All_Min100_20140314.Rdata")



