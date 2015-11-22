rankall <- function(outcome, num = "best") {
  ## Read outcome data
  file_exits <- sum(dir() == "outcome-of-care-measures.csv")
  if(file_exits == 0) stop("Invalid file")
  
  ## Check that state and outcome are valid
  if (outcome == "heart attack")
    outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  else if (outcome == "heart failure")
    outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  else if (outcome == "pneumonia")
    outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  else
    ##outcome invalid
    stop("Invalid outcome")
  
  sel_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  vect_state <- unique(sel_outcome$State)
  #convert the outcome into numbers and remove NAs 
  sel_outcome[, outcome] <- as.numeric(sel_outcome[, outcome])
  sel_outcome <- sel_outcome[!is.na(sel_outcome[, outcome]), ]
  #make sure the list is not empty
  if(nrow(sel_outcome) == 0)
    return("NA")
  else{
    #sort the data frame by state, then by outcome and then by hospital
    sel_outcome <- sel_outcome[order(sel_outcome$State, 
                                     sel_outcome[, outcome], 
                                     sel_outcome$Hospital.Name), ]
    
    result <- data.frame(hospital = c(NA), state = c(NA))
    names(result) <- c("hospital", "state")
    
    for(s in vect_state){
      state_outcome <- sel_outcome[sel_outcome$State == s, ]
      r <- rank(state_outcome[, outcome], ties.method = "first")
      if(num == "best")
        num <- 1
      else if(num == "worst")
        num <- nrow(state_outcome)
      num <- as.numeric(num)
      if(num <= nrow(state_outcome))
        hosp <- state_outcome$Hospital.Name[r == num]
      else
        hosp <- NA
      result <- rbind(result, c(hosp, s))
    }
    
    result <- result[2:nrow(result), ]
    row.names(result) <- vect_state
    return(result)
    
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}