best <- function(state, outcome) {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	## Check that state and outcome are valid
	if (!state %in% data$State) {
		stop("invalid state")
	}

	selectedCol <- NULL
	if (outcome == "heart attack") {
		selectedCol <- 11
	} else if (outcome == "heart failure") {
		selectedCol <- 17
	} else if (outcome == "pneumonia") {		
		selectedCol <- 23
	} else {
		stop ("invalid outcome")
	}

	#  Get only data with the same state value
	filteredByState = data[data$State==state,]

	#  calling suppressWarnings() to avoid "NAs introduced by coercion" message
	filteredByState[, selectedCol] <- suppressWarnings(as.numeric(filteredByState[, selectedCol]))

	#  Ordenate the data by hospital name in case of draw
	orderedByHospitalName <- filteredByState[order(filteredByState$Hospital.Name),]

	#  Get the best value (lowest) value in ordenated data
	#by Hospital name
	minVal <- min(orderedByHospitalName[, selectedCol], na.rm=TRUE)

	#  Get the best value position
	minPos <- which(orderedByHospitalName[, selectedCol] == minVal)

	## Return hospital name in that state with lowest 30-day death rate
	return (orderedByHospitalName[minPos,"Hospital.Name"])
}

#  Test function best()
# > source("best.R")
# > best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# > best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
# > best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
# > best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
# > best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
# > best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome
