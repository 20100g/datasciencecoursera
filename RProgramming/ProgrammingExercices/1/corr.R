corr <- function(directory , threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    currentDirectory <- getwd()
    setwd(directory)
    monitorsFile = list.files(".", pattern="[0-9]*.csv")
    completeCases <- complete(".", 1:length(monitorsFile))
    sulfate = NULL
    nitrate = NULL
    cr = NULL
    
    for(i in 1:length(monitorsFile)){
        ithMonitorCompleteCase <- completeCases[completeCases$id==i,]
        
        if(ithMonitorCompleteCase$nobs>threshold){
            # Read the ith monitor
            fileName <- paste (sprintf("%03d", i),".csv", sep = "")
            ithMonitorDataFrame <- read.csv(fileName)
            
            # compute correlation between sulfate and nitrate for complete cases
            ithCompleteCases <- complete.cases(ithMonitorDataFrame)
            completeCasesVal <- ithMonitorDataFrame[ithCompleteCases,]
            cr <- c(cr, cor(completeCasesVal$sulfate, completeCasesVal$nitrate))
        }
    }
    setwd(currentDirectory)
    cr
}