
# function to shuffle the list
shuffle<-function(list){
  
  list$order<-sample(seq(1:nrow(list)))
  list<-list[order(list$order),]
  
  #delete last column and create trialN column (last one)
  list<-list[,-length(list[1,])]
  #list$trialN<-seq(1, length(list[,1]))
  #trial number as first column
  #list<-list[,c(ncol(list), 1:(ncol(list)-1))]
  return(list)
}