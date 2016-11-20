rankhospital <- function(state, outcome, num = "best") {

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

	#  Remove NA values
	filteredByState <- na.omit(filteredByState)

	#  Ordenate the data by selected col
	orderedBySelectedCol <- filteredByState[order(filteredByState[,selectedCol]),]

	rankPosition <- NULL
	if (num == "best") {
		#  Get the first row
		rankPosition <- 1
	} else if (num == "worst") {
		#  Get the last row
		rankPosition <- nrow(filteredByState)
	} else if (num <= nrow(filteredByState)) {
		#  Get the received row position by argument
		rankPosition <- num
	}

	#  return NA by default
	result <- NA

	if (!is.null(rankPosition)) {

		#  Get the value using calculated row - need to check value because
		#  some values might tie
		valueAtRankPosition <- orderedBySelectedCol[rankPosition, selectedCol]

		#  Get the rows which have the value
		rowsWithSelectedValue <- which(orderedBySelectedCol[, selectedCol] == valueAtRankPosition)

		#  Sort those rows by Hospital name
		hospitalNameWithSelectedValue <- sort(orderedBySelectedCol[rowsWithSelectedValue, "Hospital.Name"])

		#  return the first of sorted data
		result <- hospitalNameWithSelectedValue[1]
	}

	## Return hospital name in that state with the given rank 30-day death rate
	return (result)
}

#  Test function rankhospital()
# > source("rankhospital.R")
# > rankhospital("TX", "heart failure", 4)
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# > rankhospital("MD", "heart attack", "worst")
# [1] "HARFORD MEMORIAL HOSPITAL"
# > rankhospital("MN", "heart attack", 5000)
# [1] NA
