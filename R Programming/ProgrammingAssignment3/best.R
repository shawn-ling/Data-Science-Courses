best <- function(state, outcome) {
  ## Check file name
  error <- FALSE
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
  best_hosp <- sel_outcome$Hospital.Name[sel_outcome[, outcome] == min(sel_outcome[, outcome])]
  if(length(best_hosp) > 1){best_hosp <- min(best_hosp)}
  best_hosp

  ## Return hospital name in that state with lowest 30-day death
  ## rate
}