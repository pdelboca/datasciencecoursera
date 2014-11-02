setwd("/home/pdelboca/Repos//datasciencecoursera/r-programming/assignment 3/")
data_dir ="./rprog-data-ProgAssignment3-data/"

file_path <- function(...){paste(data_dir,...,sep = "/")}

best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv(file = file_path("outcome-of-care-measures.csv"))
    names(data) <- gsub("\\.\\.\\.",".",names(data))
    names(data) <- gsub("\\.\\.",".",names(data))
    names(data) <- tolower(names(data))
    names(data)
    
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
}