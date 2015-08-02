complete <- function(directory,id=1:332){
  allFiles <- list.files(path=directory,full.names = TRUE)
  
  selectedDF <- data.frame()
  removeNADF <- data.frame()
  nobs <- integer()
  
  for(i in id){
    selectedDF <- read.csv(allFiles[i])
    nobs <- sum(complete.cases(selectedDF))
    removeNADF <- rbind(removeNADF,data.frame(i,nobs))
  }
  names(removeNADF)[1] <- "id"
  removeNADF
}
