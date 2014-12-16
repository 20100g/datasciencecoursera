complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    currentDirectory <- getwd()
    rowNames <- c("id", "nobs")
    completeCasesCount <- data.frame()
    
    if(file.exists(directory)) {
        setwd(directory)
                
        for(i in id){
            fileName <- paste(sprintf("%03d",i), ".csv", sep = "")
            
            if(file.exists(fileName)){
                ithDataFrame <- read.csv(file = fileName)
                count <- complete.cases(ithDataFrame )
                newRow <- c(id=i,nobs=sum(count))
                completeCasesCount <- rbind(completeCasesCount,newRow)
            }
        }
    }
    setwd(currentDirectory)
    colnames(completeCasesCount) <- rowNames
    completeCasesCount
}