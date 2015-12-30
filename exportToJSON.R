########################################################
##### Analyze traffic and booking behaviour dataset
##### Author: "Yanxia Zhang"
##### Date: "29 December 2015"
########################################################
# Input: "top10Markets" is a data table with columns (affiliate_id,top10mkt,SUM_bkgs)
#        "Closest10Partner" is a data table of the 10 closest partners' ID; each row represents a partner
#        "fileName" is the name of the output file
# Output: the results are saved into JSON format

exportToJSON <- function(top10Markets,Closest10Partner,fileName){

  partnerAmounts <- length(unique(top10Markets$affiliate_id))   #  the number of the partners (rows)
  partnerIDs <- unique(top10Markets$affiliate_id) #  the id of the partners
  
  # Create a container for listing top10 {markets vs. market booking values}
  top10 = I(vector('list', partnerAmounts)) # initialize list to have partnerAmounts null components
  
  # Create a container for listing partner vs. top 10 similar partners
  similar_partners10 = I(vector('list', partnerAmounts))
  
  # Loop through the partners (rows) to fill in the top 10 markets and closest partner information
  for (i in 1:partnerAmounts) {
    
    # Fill in top10 markets and their corresponding booking values for the current partner
    current <- top10Markets[affiliate_id == partnerIDs[i],] 
    current.SUM_bkgs <- data.frame(rbind(current$SUM_bkgs))

    # Create a data frame with each value in the diagonal position corresponding to the mkt
    # total booking values in that column
    current.SUM_bkgs <- as.data.frame(lapply(current.SUM_bkgs,rep,dim(current.SUM_bkgs)[2]))
    current.SUM_bkgs[upper.tri(current.SUM_bkgs)] <- NA 
    current.SUM_bkgs[lower.tri(current.SUM_bkgs)] <- NA
    colnames(current.SUM_bkgs) <- current$mkt # Assign mkt variables as column names 
    
    top10[[i]] <- current.SUM_bkgs
    
    similar_partners10[[i]] <- Closest10Partner[partnerIDs[i],]
    
  }
  
  # Create a data frame to store the results
  myDF <- data.frame(partner_name = partnerIDs, 
                     top10Markets = top10, 
                     similar_partners = similar_partners10)
  
  # Output the results to a JSON file 
  exportJson <- jsonlite::toJSON(myDF, pretty = TRUE)
  write(exportJson, fileName)
  
}
