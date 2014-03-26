##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Join the "store" and "features" data to the "train" dataset
##  2. Create some utilitarian variables
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(plyr)

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/walmart/data/current")

##------------------------------------------------------------------
## Load the raw Rdata data
##------------------------------------------------------------------
## features.raw	<- additional store/region data
## stores.raw	<- store-specific data
## train.raw	<- training dataset
## test.raw		<- test dataset
##------------------------------------------------------------------
load("001_walmartRawData.Rdata")

##------------------------------------------------------------------
## Copy the raw data to new objects
##------------------------------------------------------------------
test		<- test.raw
train 		<- train.raw
stores		<- stores.raw
features	<- features.raw

##------------------------------------------------------------------
## Lowercase the headers
##------------------------------------------------------------------
names(test) 	<- tolower(names(test))
names(train) 	<- tolower(names(train))
names(stores) 	<- tolower(names(stores))
names(features) <- tolower(names(features))

##------------------------------------------------------------------
## Combine the test and training data into a single dataframe (comb)
##------------------------------------------------------------------

## create a dummy sales column for the test data and re-order cols
test$weekly_sales 	<- 0
test				<- test[ , c(1,2,3,5,4) ]

## Create train/test id(s) 
train$t_id	<- c("TR")
test$t_id	<- c("TE")

## combine
comb.raw	<- rbind(train, test)	

##------------------------------------------------------------------
## Append store data (by StoreId)
##------------------------------------------------------------------
comb <- join(comb.raw, stores, by="store")

##------------------------------------------------------------------
## Append region/economic data t(by Date, StoreId; train only)
##------------------------------------------------------------------

## Create unique keys (Store, Date)
comb$storeDateKey		<- paste(comb$store, comb$date, sep="_")
features$storeDateKey	<- paste(features$store, features$date, sep="_")

## Rename the duplicate fields
origNames		<- c("store", "dept", "date", "isholiday")
newNames		<- gsub("$", ".o", origNames )
names(comb)[which(names(comb) %in% origNames)]	<- newNames

## Merge
comb <- join(comb, features, by="storeDateKey")

## Drop the appended, duplicate fields
comb$store			<- NULL
comb$date			<- NULL
comb$isholiday		<- NULL
comb$storeDateKey	<- NULL

## Back to original names
names(comb)	<- gsub(".o$", "", names(comb))


##------------------------------------------------------------------
## Create an integer week index (train,test)
##------------------------------------------------------------------
comb$nweek		<- as.numeric(format(as.Date(comb$date, format="%Y-%m-%d"), format="%V"))


##------------------------------------------------------------------
## Create a store/dept index (train,test)
##------------------------------------------------------------------
combStoreChar	<- ifelse(comb$store < 10, paste("0",comb$store,sep=""), comb$store)
combDeptChar	<- ifelse(comb$dept < 10, paste("0",comb$dept,sep=""), comb$dept)
comb$sd_idx		<- as.factor(paste(combStoreChar,"_",combDeptChar,sep=""))

##------------------------------------------------------------------
## Identify unique store/dept combinations
##------------------------------------------------------------------

## unique fields for *all* data (train/test)
uniqStores		<- sort(unique(comb$store))
uniqDept		<- sort(unique(comb$dept))
uniqSd			<- sort(unique(comb$sd_idx))
numStores		<- length(uniqStores)
numDept			<- length(uniqDept)
numSd			<- length(uniqSd)

## unique fields for *test* data only
uniqTestStores	<- sort(unique(comb[ which(comb$t_id == "TE"), c("store")]))
uniqTestDept	<- sort(unique(comb[ which(comb$t_id == "TE"), c("dept")]))
uniqTestSd		<- sort(unique(comb[ which(comb$t_id == "TE"), c("sd_idx")]))
numTestStores	<- length(uniqTestStores)
numTestDept		<- length(uniqTestDept)
numTestSd		<- length(uniqTestSd)

## store the unique fields as a list
uniq.list		<- list(
						uniqStores=uniqStores,
						uniqDept=uniqDept,
						uniqSd=uniqSd,
						numStores=numStores,
						numDept=numDept,
						numSd=numSd,
						uniqTestStores=uniqTestStores,
						uniqTestDept=uniqTestDept,
						uniqTestSd=uniqTestSd,
						numTestStores=numTestStores,
						numTestDept=numTestDept,
						numTestSd=numTestSd
					)
					
##------------------------------------------------------------------
## Save results
##------------------------------------------------------------------
save(comb, train, test, stores, features, uniq.list, file="002_walmartCombinedData.Rdata")

