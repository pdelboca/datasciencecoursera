data_dir ="./rprog-data-ProgAssignment3-data/"

library(datasets) # to test valid states

file_path <- function(...){paste(data_dir,...,sep = "/")}
valid_outcomes <- c("heart attack", "heart failure", "pneumonia")

rankhospital <- function(state,outcome,rank){
    ## Check that state and outcome are valid
    if(!(state %in% state.abb)) {stop("invalid state")}
    if(!(outcome %in% valid_outcomes)) {stop("invalid outcome")}
    
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
    
    # Cast as.double so order function can works properly
    data[,outcome] <- as.double(data[,outcome])
    
    # Order the rows by the outcome column and then by hospital name
    data <- data[ order(data[,outcome],data$hospital.name), ]
    
    # Return data depending on the rank input
    if(rank == "best") {hospital <- data[1,"hospital.name"]}
    else if(rank == "worst") {hospital <- data[nrow(data),"hospital.name"]}
    else {hospital <- data[rank,"hospital.name"]}
    
    hospital
}

5Cb298k9yu

rankhospital("TX", "heart failure", 4) # [1] "DETAR HOSPITAL NAVARRO"
rankhospital("MD", "heart attack", "worst") # [1] "HARFORD MEMORIAL HOSPITAL"
rankhospital("MN", "heart attack", 5000) # [1] NA