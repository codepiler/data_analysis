########################################################
##### Analyze traffic and booking behaviour dataset
##### Author: "Yanxia Zhang"
##### Date: "29 December 2015"
########################################################
# Input: data with columns (affiliate_id,mkt,SUM_bkgs...)
# Output: data with columns (affiliate_id,top10mkt,SUM_bkgs)
# Description: Extact the top 10 market locations sorted by the total of booking value

findTop10Markets <- function(dt.groupByPartnerMarket){
  
  # Apply the get top 10 function to subsets of the data belong to different "affiliate_id"
  dt.top10markets <- do.call(rbind, 
                             by(dt.groupByPartnerMarket, INDICES=dt.groupByPartnerMarket$affiliate_id, 
                                FUN=function(x) head(x[order(-x$SUM_bkgs)],10))) # Group by "affiliate_id"
  
  return(dt.top10markets[,.(affiliate_id,mkt,SUM_bkgs)])
  
}