pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## Calculates the mean of a pollutant (sulfate or nitrate) across a 
    ## specified list of monitor
    
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    
    if(file.exists(directory)){
        currentDirectory <- getwd()
        setwd(directory)
        completeDataFrame <- data.frame()
        
        for(i in id){
            ithFileName = paste(sprintf("%03d",i),".csv", sep = "")
            ithDataFrame = read.csv(ithFileName)
            completeDataFrame <- rbind(completeDataFrame, ithDataFrame)
        }
        
        
        if(pollutant =="nitrate" || pollutant =="sulfate"){
            pollutantMean <- mean(completeDataFrame[[pollutant]], na.rm = TRUE);
        }
        setwd(currentDirectory)
        pollutantMean
    }
    
}