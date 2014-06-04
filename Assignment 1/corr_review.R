corr <- function(directory, threshold = 0) {
  
  #id <- 332:1
  id <- 1:332                                                           # this appears to get the right answer
  id <- formatC(id,width = 3,format = "d", flag = "0")
  
  filepath <- paste(directory,"/",id,".csv",sep = "")
  alldata <- c()
  
  for (path in filepath){
    tempdata <- read.csv(path)
    alldata <- rbind(alldata,tempdata)
  }
  
  
  a <-subset(alldata,!is.na(sulfate) & !is.na(nitrate))
  
  #reverse order
  b <- ddply(a,c("ID"),function(x) c(count=nrow(x)))                    # could have used earlier 'complete' function

  c <- cbind(b,as.integer(b$ID))                                        # but ID is already an integer
  d <- rename(c, c("as.integer(b$ID)"="id"))

  idframe <- data.frame(as.integer(id))
  e <- rename(idframe, c("as.integer.id."="id"))

  lasttable <- join(e,d,by = "id",type = "left",match = "first")
  FINAL <- rename(lasttable, c("count"="nobs"))                         # how is this different from b except in reverse order
  
  FINAL2 <- FINAL[c("id","nobs")]                                       # just removed dup id
  
  FINAL2[is.na(FINAL2)] <- 0                                            # Any na's were removed in a
  
  # return(FINAL2)                                                      # just use b
  
  idselect <- subset(FINAL2,nobs > threshold)
  idfilter <- idselect$id
  
  corrvector <- c()
  
  for(no in idfilter) {
    
    x <- subset(a,ID == no)
    sulfate <- x$sulfate
    nitrate <- x$nitrate
    
    tempcorr <- cor(sulfate,nitrate)
    corrvector <- append(corrvector,tempcorr)
    
  }
  return(corrvector)            # at a glance appears to produce the right answer except in reverse order
}
