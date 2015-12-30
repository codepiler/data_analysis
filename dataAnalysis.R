########################################################
##### Analyze traffic and booking behaviour dataset
##### Author: "Yanxia Zhang"
##### Date: "29 December 2015"
########################################################
#Step 1. Read in the ".csv" data file into memory 
#Step 2. Find partner names by looking for unique affiliate_id
#Step 3. Find the top 10 markets per partner
#Step 4. Find the top 10 simialr partners per partner
#Step 5. Output the results into a JSON file

rm(list=ls(all=TRUE))

# Install these packages
library(data.table)
library(jsonlite)
library(ggplot2)
library(reshape2)
library(lsa)

source("exportToJSON.R")
source("find10ClosestPartners.R")
source("findTop10Markets.R")

# Set the current directory to "source file location"
setwd(".")

########################################################
######Step 1. Read in the ".csv" data file into memory 
########################################################

# Read in the data
casestudy_df <- read.csv("data/intern_casestudy_data.csv", header = TRUE)
summary(casestudy_df)

# Check the dimensions,type and structure of the data
str(casestudy_df)

# Count missing values by counting the null values 
sum(is.na(casestudy_df)) 

########################################################
###Step 2. Find partner names by looking for unique affiliate_id
########################################################
# Save all partners' ID in "partnerIDs" 
partnerIDs <- unique(casestudy_df$affiliate_id)

# Convert a data.frame into a data.table
casestudy_dt <- data.table(casestudy_df)

# Set "affiliate_id" and "mkt" as keys so we can group the data by these values 
setkey(casestudy_dt,affiliate_id, mkt)

# Booking values are grouped by partner 'affiliate_id' and market 'mkt'
# Add three new columns to the grouped data: SUM_bkgs, MEAN_bkgs, COUNT_bkgs  
dt.groupByPartnerMarket <- casestudy_dt[,
                                        list("SUM_bkgs"=sum(bkgs),       # SUM_bkgs: total booking values 
                                             "MEAN_bkgs"=mean(bkgs),     # MEAN_bkgs: average booking values
                                             "COUNT_bkgs"=length(bkgs)), # COUNT_bkgs: number of transactions
                                        list(affiliate_id,mkt)] 


########################################################
####Step 3. Find the top 10 markets per partner
########################################################
# For each group, extract the top 10 markets by summing up the booking 
# value per market destination

dt.partners.top10Markets <- findTop10Markets(dt.groupByPartnerMarket) 

###########################################################
####Step 4. Find the top 10 simialr partners per partner
###########################################################

# Reshape the data to "partners (rows) vs. booking values per market (columns)"
dt.partnerMarketDistribution <- dcast(dt.groupByPartnerMarket,
                                      affiliate_id~mkt, 
                                      value.var = 'SUM_bkgs' )

# Replace "NA" no booking values with 0
dt.partnerMarketDistribution[is.na(dt.partnerMarketDistribution)] <- 0

# Drop the partner ID (column 1)
# Create a new datatable with partner ID column as row names
firstColumn <- dt.partnerMarketDistribution$affiliate_id
dt.partnerMarketDistribution$affiliate_id = NULL
rownames(dt.partnerMarketDistribution) <- firstColumn

# Use function "find10Closest" to find the top 10 similar 
# partners based on booking value distributions across the markets
dt.partnerMarketDistribution.10ClosestPartners <- find10ClosestPartners(dt.partnerMarketDistribution)

########################################################
####Step 5. Output results into a JSON file "task1.JSON"
########################################################
exportToJSON(dt.partners.top10Markets,dt.partnerMarketDistribution.10ClosestPartners, "task1.JSON")
