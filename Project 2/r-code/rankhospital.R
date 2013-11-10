rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data: COLS: HospitalName, State, HeartAttack, HearFailure, Pneumonia
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,c(2,7,11,17,23)]
  
  ## Check that state and outcome are valid
  if(! ( state %in% levels(factor(data$State)) ) ) {
    stop("invalid state")
  }
  
  if(! (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") ) {
    stop("invalid outcome")
  }
  
  if(class(num) == "character"){
    if (! (num == "best" || num == "worst")){
      stop("invalid number")
    }
  }
  
  ## Return hospital name in that state with the given rank 30-day death rate
  
  # Remove row by state and columns state
  data = data[data$State==state,]
  data = data[,c(1,3,4,5)]
  
  # Remove columns by outcome, only left HospitalName and Deaths by outcome
  if(outcome == "heart attack") {
    data = data[,c(1,2)]
  } else if(outcome == "heart failure") {
    data = data[,c(1,3)]
  } else if(outcome == "pneumonia") {
    data = data[,c(1,4)]
  }
  names(data)[2] = "Deaths"
  data[, 2] = suppressWarnings( as.numeric(data[, 2]) )
  
  # Remove rows with NA
  data = data[!is.na(data$Deaths),]
  
  # if num > numrows then return NA
  if(class(num) == "numeric" && num > nrow(data)){
    return (NA)
  }
  
  # Order by Deaths and then HospitalName
  data = data[order(data$Deaths, data$Hospital.Name),]
  
  # Return
  if(class(num) == "character") {
    if(num == "best") {
      return (data$Hospital.Name[1])
    }
    else if(num == "worst") {
      return (data$Hospital.Name[nrow(data)])
    }
  }
  else {
    return (data$Hospital.Name[num])
  }
}