########################################################
##### Analyze traffic and booking behaviour dataset
##### Author: "Yanxia Zhang"
##### Date: "29 December 2015"
########################################################
# Input: a data table of total booking values with rows (id of the partner) x columns (id of the market destinations)
# Output: a data table of the 10 closest partners' ID; each row represents a partner
# Description: Extract the top 10 similar partners sorted by the cosine similarties between 
# booking value distributions across the market. 

find10ClosestPartners <- function(dt.mktDistribution) {
  
  # Create an empty dataframe listing similarities of partner vs. partner
  dt.mktDistribution.similarity <- matrix(NA, nrow=nrow(dt.mktDistribution),ncol=nrow(dt.mktDistribution),
                   dimnames=list(rownames(dt.mktDistribution),rownames(dt.mktDistribution)))
  
  # Compute the similarities between all row vectors (market booking value distribution)
  # Store the cosine similarties to the data frame with each element [i,j] represents similarity 
  # score between partner i and j. 
  dt.mktDistribution.similarity <- cosine(t(as.matrix(dt.mktDistribution)))
  df.similarity <- data.frame(dt.mktDistribution.similarity)
  
  # Get the top 10 closest partners for each partner based on the similarity score
  dt.mktDistribution.top10similar <- matrix(NA, 
                                            nrow=nrow(df.similarity),
                                            ncol=11,dimnames=list(rownames(df.similarity)))
  
  # Loop through each partner to extract top 10 partners' ID 
  for(i in 1:nrow(dt.mktDistribution)) 
  {
    dt.mktDistribution.top10similar[i,] <- (t(head(n=11,rownames(df.similarity[order(df.similarity[,i],decreasing=TRUE),][i]))))
  }
  
  return(dt.mktDistribution.top10similar[,-1])
  
}