best <- function(state, outcome) {
    ## Read outcome data
    setwd("C:/Users/i045177/Documents/GitHub/ProgrammingAssignment3")
    all_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    state_outcome = subset(all_outcome, 
                         State == state
    )
    if(nrow(state_outcome) > 0){ #if the state is in the list
        if(outcome == "heart attack")
            outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        else if(outcome == "heart failure")
            outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        else if(outcome == "pneumonia")
            outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        else
            x = 1       ##outcome invalid
        n <- names(state_outcome)
        u1 <- n == "State"
        u2 <- n == "Hospital.Name"
        u3 <- n == outcome
        u = (u1 | u2 | u3)
        sel_outcome = state_outcome[u]
        good = subset(sel_outcome, sel_outcome$outcome != "Not Available" )
        x = 1
    }
    else{
        x = 1           ##state in valide
    }

    ## Return hospital name in that state with lowest 30-day death
    ## rate
}