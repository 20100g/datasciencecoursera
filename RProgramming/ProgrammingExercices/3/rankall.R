rankall <- function(outcome, num = "best"){
    # Finds the hospital having the lowest 30-day mortality rate for a specific
    # outcome ("heart attack", "heart failure", or "pneumonia) in a given state
    #
    # Args:
    #   state: A 2 character code identifying the state
    #   outcome: A character vector the outcome 
    #            ("heart attack", "heart failure", or "pneumonia)
    #
    # Returns:
    #   The name of the hospital having the lowest 30-day mortality rate for a 
    #   specific outcome ("heart attack", "heart failure", or "pneumonia) in a 
    #   given state
    
    # Read the csv file
    outcomeDS <- read.csv("outcome-of-care-measures.csv", 
                               stringsAsFactors = FALSE)
    
    # Assign list of valid states and mortality causes (outcomes)
    states <- unique(outcomeDS$State)
    mortalityCausesDef <- c("heart attack"= 11, 
                           "heart failure"= 17, 
                           "pneumonia" = 23)
    ranks <- c("best","worst")
            
    # Check that the outcome and rank (num) are valid
    if(sum(names(mortalityCausesDef)==outcome)==0){
        stop("invalid outcome")
    }
    if(!is.numeric(num) & sum(num==ranks)==0){
        stop("invalid rank. Should be 'best', 'worst' or a numeric value")
    }
    
    # gets the mortality rate for the specific outcome 
    mortalityCol <- as.numeric(outcomeDS[,mortalityCausesDef[outcome]])
    
    # gets a subset of the original dataset
    # for the specific outcome and state (ignoring NAs)
    HospWData <- subset(outcomeDS,!is.na(mortalityCol))
    
    #TODO : investigate use of by()
    # split by state
    hospWDataByState <- split(HospWData, HospWData$State)
    lst <- lapply(hospWDataByState, function(ele){
        #orders list by mortality outcome then hospital name
       ordered <- ele[order(as.numeric(ele[,mortalityCausesDef[outcome]]), 
                            ele$Hospital.Name),]
       
       rank <- if(num=="best"){
           1
       }else if (num=="worst"){
           dim(ordered)[1]
       }else if(num > dim(ordered)[1]){
           return(data.frame(hospital=as.character("NA"),
                             state=as.character(ordered[1,]$State)))
       }
       else {
           num
       }
       
       hosp <- ordered[rank,c("State","Hospital.Name")]
       hoptTrsf <- transform(hosp, hospital = Hospital.Name, state = State)

       return(hoptTrsf[,c("hospital","state")])
       
    } )
    
    # collapses the list into a dataframe
    out <- do.call(rbind,lst)
    out
    


}