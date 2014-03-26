##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Read the raw data files
##------------------------------------------------------------------

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/walmart/data/current")

##------------------------------------------------------------------
## Read in the various files
##------------------------------------------------------------------
features.raw	<- read.csv("/Users/alexstephens/Development/kaggle/walmart/data/kaggle_raw/features.csv", header=TRUE)
stores.raw		<- read.csv("/Users/alexstephens/Development/kaggle/walmart/data/kaggle_raw/stores.csv", header=TRUE)
train.raw		<- read.csv("/Users/alexstephens/Development/kaggle/walmart/data/kaggle_raw/train.csv", header=TRUE)
test.raw		<- read.csv("/Users/alexstephens/Development/kaggle/walmart/data/kaggle_raw/test.csv", header=TRUE)

##------------------------------------------------------------------
## Write the raw data to an .Rdata file
##------------------------------------------------------------------
save(features.raw, stores.raw, train.raw, test.raw, file="001_walmartRawData.Rdata")
