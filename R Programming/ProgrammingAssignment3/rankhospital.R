rankhospital <- function(state, outcome, num = "best") {

  ## Check that state and outcome are valid
  ## Check file name

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
  sel_outcome <- sel_outcome[sel_outcome$State == state, ]
  ##No data found for the state input
  if(nrow(sel_outcome) == 0) stop("Invalid state")
  
  sel_outcome[, outcome] <- as.numeric(sel_outcome[, outcome])
  sel_outcome <- sel_outcome[!is.na(sel_outcome[, outcome]), ]
  if(nrow(sel_outcome) == 0)
    return("NA")
  else{
    sel_outcome <- sel_outcome[order(sel_outcome[, outcome], sel_outcome$Hospital.Name), ]
    if(num == "best")
      num <- 1
    else if(num == "worst")
      num <- nrow(sel_outcome)
    num <- as.numeric(num)
    if(num > nrow(sel_outcome) | num < 1) return("NA")
    hosp <- sel_outcome$Hospital.Name[num]
    return(hosp)
  }

  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}