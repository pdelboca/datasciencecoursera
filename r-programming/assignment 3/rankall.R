data_dir ="./rprog-data-ProgAssignment3-data/"

library(datasets) # to test valid states

file_path <- function(...){paste(data_dir,...,sep = "/")}
valid_outcomes <- c("heart attack", "heart failure", "pneumonia")

rankall <- function(outcome,rank="best"){
    ## Check that state and outcome are valid
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

    data <- data[,c("hospital.name","state",outcome)]
    names(data) <- c("hospital.name","state","outcome")
    
    hospitals <- character(0)
    states <- character(0)
    outcomes <- double(0)
    
    for(state in unique(data$state)){
        # Filter data by the state and only valid values
        state_data <- data[data$state == state & data$outcome != "Not Available",]
        
        # Cast as.double so order function can works properly
        state_data$outcome <- as.double(state_data$outcome)
        
        # Order the rows by the outcome column and then by hospital name
        state_data <- state_data[ order(state_data$outcome,
                                        state_data$hospital.name), ]
        
        # Return data depending on the rank input
        if(rank == "best") { hospital <- state_data[1,"hospital.name"] }
        else if(rank == "worst") { hospital <- state_data[nrow(state_data),"hospital.name"] }
        else { hospital <- state_data[rank,"hospital.name"] }
        
        hospitals <- c(hospitals,hospital)
        states <- c(states,state)
    }
    
    ranked_hospitals = data.frame(hospitals,states)
    ranked_hospitals <- ranked_hospitals[ order(ranked_hospitals$state) , ]
    names(ranked_hospitals) <- c("hospital","state")
    ranked_hospitals
}


head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)