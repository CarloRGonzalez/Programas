setwd("~/Desktop/Online Coursera/Coursera-R-Programming/week4/")
list.files()

helper <- function(data, col_num, state) {
  state_subset <- data[data[, 7]==state, ]
  outcome_arr <- state_subset[, col_num]
  min <- min(outcome_arr, na.rm=T)
  min_index <- which(outcome_arr == min)
  hosp_name <- state_subset[min_index, 2]
  return(hosp_name)
}

best <- function(state, outcome) {

  directory <- "C:/Users/Luis Muñoz/Desktop/coursera/outcome-of-care-measures.csv"
  data <- read.csv(directory, colClasses="character")
 
  data[, 11] <- as.numeric(data[, 11]) 
  data[, 17] <- as.numeric(data[, 17]) 
  data[, 23] <- as.numeric(data[, 23]) 
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!state %in% data$State) {
    stop("invalid state")
  } else if(!outcome %in% valid_outcomes) {
    stop("invalid outcome")
  } else {
    if(outcome == "heart attack") {
      hosp_name <- helper(data, 11, state)
    } else if(outcome == "heart failure") {
      hosp_name <- helper(data, 17, state)
    } else {
      hosp_name <- helper(data, 23, state)
    }
    result <- hosp_name
    return(result)
  }
}
rankhospital <- function(state, outcome, num = "best") {
  full_data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  column <- if (outcome == "heart attack") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else {
    stop("invalid outcome")
  }
  
  data_for_state <- full_data[full_data$State == state, c("Hospital.Name", column)]
  
  if (nrow(data_for_state) == 0) {
    stop("invalid state")	
  }
  
  data_for_state[,2] <- as.numeric(data_for_state[,2])
  ordered_data_for_state <- order(data_for_state[column], data_for_state$Hospital.Name, na.last=NA)
  
  if (num == "best") {
    as.character(data_for_state$Hospital.Name[ordered_data_for_state[1]])
  } else if (num == "worst") {
    as.character(data_for_state$Hospital.Name[ordered_data_for_state[length(ordered_data_for_state)]])
  } else if (is.numeric(num)) {
    as.character(data_for_state$Hospital.Name[ordered_data_for_state[num]])
  } else {
    stop("invalid num")
  }
}
rankall <- function(outcome, num = "best" ){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state <- data$State
  state <- sort(unique(state))
  hospital <- rep("", length(state))
  for (i in 1:length(state)) {
    statedata<- data[data$State==state[i],]
    if (outcome == 'heart attack') {
      death <- as.numeric(statedata[,11])
    } else if (outcome == 'heart failure') {
      death <- as.numeric(statedata[,17])
    } else if (outcome == 'pneumonia') {
      death <- as.numeric(statedata[,23])
    } else {
      stop("invalid outcome")
    }
    
    a <- rank(death, na.last=NA)
    
    if (num=="best") {
      r <- 1
    } else if (num =="worst") {
      r <- length(a)
    } else if (num <= length(a) ) {
      r <- num
    } else {
      r <- NA
    }
    
    if (is.na(r)) {
      hospital[i] <- NA
    } else {
      hospital[i] <- statedata$Hospital.Name[order(death, statedata$Hospital.Name)[r]]
    }
    
  }
  
  return(data.frame(hospital=hospital, state=state))
}
}