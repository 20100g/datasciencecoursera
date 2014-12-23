rankhospital <- function(state, outcome, num = "best"){
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
            
    # Check that the outcome and state are valid
    if(sum(states==state)==0){
        stop("invalid state")
    }
    if(sum(names(mortalityCausesDef)==outcome)==0){
        stop("invalid outcome")
    }
    if(!is.numeric(num) & sum(num==ranks)==0){
        stop("invalid rank . Should be 'best' worst' or a numeric value")
    }
    
    # gets the mortality rate for the specific outcome 
    mortalityCol <- as.numeric(outcomeDS[,mortalityCausesDef[outcome]])
    
    # gets a subset of the original dataset
    # for the specific outcome and state (ignoring NAs)
    stHospWData <- subset(outcomeDS, State == state & !is.na(mortalityCol))


    
    # calculate the rank
    
        rank <- if(num=="best"){
            1
        }else if (num=="worst"){
            dim(stHospWData)[1]
        }else if(num > dim(stHospWData)[1]){
            return(NA)
        }
        else {
            num
        }
        
    # order the dataset by the mortality rate (ascending) with ties 
    # being broken by the hospital name
    orderedHosp <- stHospWData[order(as.numeric(stHospWData[,mortalityCausesDef[outcome]]),
                      stHospWData$Hospital.Name),]
    
    
    orderedHosp[rank,]$Hospital.Name
}