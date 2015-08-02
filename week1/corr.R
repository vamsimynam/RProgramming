corr <- function(directory,threshold=0){
  source('complete.R')
  removeNaDf <- complete(directory)
  
  GtThresholdFiles <- removeNaDf[removeNaDf$nobs > threshold,1]
  
  allFiles <- list.files(path=directory,full.names = TRUE)
  
  correlation <- rep(NA,length(GtThresholdFiles))
  for(i in GtThresholdFiles){
    selectedGtThldFile <- read.csv(allFiles[i])
    
    completeCases <- complete.cases(selectedGtThldFile)
    sulfateData <- selectedGtThldFile[completeCases,2]
    nitrateData <- selectedGtThldFile[completeCases,3]
    
    correlation[i] <- cor(sulfateData,nitrateData)
  }
  correlation <- correlation[complete.cases(correlation)]
}
