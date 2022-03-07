getSum<-function(df){
  #----------------------------------------------------------------------------#
  # function that check the number of incongruent consecutive trials
  # (regardless of the character)
  # It takes into account the "type" variable, which is 0 for incongruent and 1
  # for congruent
  # 
  #   INPUT: df - dataframe with all the choices
  #   OUTPUTL: a vector with the sum of three consecutive "type" variable
  #----------------------------------------------------------------------------#
  
  #sums<-vector()
  #for (n in 1: length(characters)){
    
    # select the trials for one character
    #currList<-filter(df, character == characters[n])
    currList<-df
    # sum three consecutive trials
    sum<-vector()
    for (j in 3:nrow(currList)){
      sum[j]<-sum(currList$type[(j-2):j])
    }
    #sums<-c(sums, sum)  
  #}
  
  return(sum)
}