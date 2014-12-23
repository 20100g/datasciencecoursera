best <- function(state, outcome){
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
            
    # Check that the outcome and state are valid
    if(sum(states==state)==0){
        stop("invalid state")
    }
    if(sum(names(mortalityCausesDef)==outcome)==0){
        stop("invalid outcome")
    }
    
    # TODO : probably a much easier way to do this
    
    # gets the mortality rate for the specific outcome 
    mortalityCol <- as.numeric(outcomeDS[,mortalityCausesDef[outcome]])
    
    # gets the minimum mortality rate by state for the outcome
    minMortalityByState <- tapply(mortalityCol,outcomeDS$State,min, na.rm = TRUE)


    mortalityRate <-  minMortalityByState[state]

    # Find the best hospital
    bestHospitals <- outcomeDS[
        which(outcomeDS$State == state & mortalityCol==mortalityRate ),]
     
    if(length(bestHospitals)>1){
        return(bestHospitals[1,]$Hospital.Name)
    }
    
    bestHospitals$Hospital.Name
}