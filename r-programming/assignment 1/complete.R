# PROGRAMMING ASSIGMENT 1: AIR POLLUTION
# PART 2
# Write a function that reads a directory full of files and reports the number of 
# completely observed cases in each data file. The function should return a data 
# frame where the first column is the name of the file and the second column is 
# the number of complete cases. A prototype of this function follows


complete <- function(directory, id = 1:332) {
    # Get full path of the specsdata folder
    directory <- paste(getwd(),"/","specdata","/",sep="")
    
    # Aux variables
    file_list <- list.files(directory)
    ids <- vector()
    nobs <- vector()
    
    #For each id passed as parameter:
    for (i in id) {
        # Read the file,
        file_dir <- paste(directory,file_list[i],sep="")
        file_data <- read.csv(file_dir)
        
        # acumulate ids and nobs values in the vectors    
        ids = c(ids,i)
        nobs = c(nobs,sum(complete.cases(file_data)))        
    }
    # Finally, Create the data frame using the vectors and return it
    data.frame(id = ids, nobs = nobs)
}


# >============== TEST SCENARIOS ==============<

test1 <- complete("specdata", 1)
test1
##   id nobs
## 1  1  117
test2 <- complete("specdata", c(2, 4, 8, 10, 12))
test2
##   id nobs
## 1  2 1041
## 2  4  474
## 3  8  192
## 4 10  148
## 5 12   96
test3 <- complete("specdata", 30:25)
test3
##   id nobs
## 1 30  932
## 2 29  711
## 3 28  475
## 4 27  338
## 5 26  586
## 6 25  463
test4 <- complete("specdata", 3)
test4
##   id nobs
## 1  3  243