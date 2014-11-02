data_dir ="./rprog-data-ProgAssignment3-data/"

library(datasets) # to test valid states

file_path <- function(...){paste(data_dir,...,sep = "/")}
valid_outcomes <- c("heart attack", "heart failure", "pneumonia")


## Return hospital name in that state with lowest 30-day death rate for the 
## given outcome
best <- function(state, outcome) {
    ## Check that state and outcome are valid
    if(!(state %in% state.abb)) {stop("invalid state")}
    if(!(outcome %in% valid_outcomes)) {stop("invalid outcome")}
        
    ## colClasses is mandatory to use which.min() method
    data <- read.csv(file = file_path("outcome-of-care-measures.csv"),
                     colClasses="character")
    ## Also give readable names to the columns
    names(data) <- gsub("\\.\\.\\.",".",names(data))
    names(data) <- gsub("\\.\\.",".",names(data))
    names(data) <- tolower(names(data))
        
    # Format and Paste the outcome to access directly to the target column 
    outcome <- gsub(" ",".",outcome)
    outcome <- paste("hospital.30.day.death.mortality.rates.from",outcome,
                     sep=".")
    
    # Row subets matching state and without "Not Available" values
    data <- data[data$state == state & data[outcome] != "Not Available", ]
    
    # Returns the row in which the outcome has the minium value
    data <- data[which.min(data[ ,outcome]), ]
    hospital_name <- data[ ,"hospital.name"]
    hospital_name
}

#best("TX", "heart attack") # [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
#best("TX", "heart failure") # [1] "FORT DUNCAN MEDICAL CENTER"
#best("MD", "heart attack") # [1] "JOHNS HOPKINS HOSPITAL, THE"
#best("MD", "pneumonia") # [1] "GREATER BALTIMORE MEDICAL CENTER"
#best("BB", "heart attack") # Error in best("BB", "heart attack") : invalid state
#best("NY", "hert attack") # Error in best("NY", "hert attack") : invalid outcome
