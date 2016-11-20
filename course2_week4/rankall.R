get_data_by_disease <- function(stateSubset, colNum, num) {
	filteredRate <- as.numeric(stateSubset[, colNum])
	len <- dim(stateSubset[!is.na(filteredRate), ])[1]
	if (num == "best") {
		rank <- select_hospital_by_num(stateSubset, filteredRate, 1)
	} else if (num == "worst") {
		rank <- select_hospital_by_num(stateSubset, filteredRate, len)
	} else if (num > len) {
		rank <- NA
	} else {
		rank <- select_hospital_by_num(stateSubset, filteredRate, num)
	}
	result <- rank
	return(result)
}

select_hospital_by_num <- function(stateSubset, filteredRate, num) {
	result <- stateSubset[, 2][order(filteredRate, stateSubset[, 2])[num]]
	return(result)
}

rankall <- function(outcome, num = "best") {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses="character")

	## Check that outcome are valid
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

	#  Get the list of states
	allUniqueState <- sort(unique(data$State))

	#  Get how many states have
	numberOfAllStates <- length(allUniqueState)

	#  Allocate output
	hospital <- rep("", numberOfAllStates)
	
	## For each state, find the hospital of the given rank
	for(i in 1:numberOfAllStates) {
		stateSubset <- data[data$State==allUniqueState[i], ]
		hospital[i] <- get_data_by_disease(stateSubset, selectedCol, num) 
	}

	## Return a data frame with the hospital names and the (abbreviated) state name    	
	result <- data.frame(hospital=hospital, state=allUniqueState)
	return(result)
}

#  Test function rankall()
# > source ("rankall.R")
# There were 50 or more warnings (use warnings() to see the first 50)
# > head(rankall("heart attack", 20), 10)
#                               hospital state
# 1                                 <NA>    AK
# 2       D W MCMILLAN MEMORIAL HOSPITAL    AL
# 3    ARKANSAS METHODIST MEDICAL CENTER    AR
# 4  JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
# 5                SHERMAN OAKS HOSPITAL    CA
# 6             SKY RIDGE MEDICAL CENTER    CO
# 7              MIDSTATE MEDICAL CENTER    CT
# 8                                 <NA>    DC
# 9                                 <NA>    DE
# 10      SOUTH FLORIDA BAPTIST HOSPITAL    FL
# There were 50 or more warnings (use warnings() to see the first 50)
# > tail(rankall("pneumonia", "worst"), 3)
#                                      hospital state
# 52 MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
# 53                     PLATEAU MEDICAL CENTER    WV
# 54           NORTH BIG HORN HOSPITAL DISTRICT    WY
# There were 46 warnings (use warnings() to see them)
# > tail(rankall("heart failure"), 10)
#                                                             hospital state
# 45                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL    TN
# 46                                        FORT DUNCAN MEDICAL CENTER    TX
# 47 VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER    UT
# 48                                          SENTARA POTOMAC HOSPITAL    VA
# 49                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR    VI
# 50                                              SPRINGFIELD HOSPITAL    VT
# 51                                         HARBORVIEW MEDICAL CENTER    WA
# 52                                    AURORA ST LUKES MEDICAL CENTER    WI
# 53                                         FAIRMONT GENERAL HOSPITAL    WV
# 54                                        CHEYENNE VA MEDICAL CENTER    WY
# There were 50 or more warnings (use warnings() to see the first 50)
