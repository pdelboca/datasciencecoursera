# PROGRAMMING ASSIGMENT 1: AIR POLLUTION
# PART 3
# Write a function that takes a directory of data files and a threshold for 
# complete cases and calculates the correlation between sulfate and nitrate for 
# monitor locations where the number of completely observed cases (on all 
# variables) is greater than the threshold. The function should return a vector 
# of correlations for the monitors that meet the threshold requirement. If no
# monitors meet the threshold requirement, then the function should return a 
# numeric vector of length 0.

source("complete.R")

corr <- function(directory, threshold = 0) {
    # Get full path of the specsdata folder
    directory <- paste(getwd(),"/",directory,"/",sep="")    
    
    #Get observations and filter by threshold
    observations <- complete(directory)
    filtered_observations = subset(observations,observations$nobs > threshold)
        
    # Aux variables
    file_list <- list.files(directory)
    correlation <- vector()
    
    # For each id in filtered observations:
    for (i in filtered_observations$id) {
        # Read the file,
        file_dir <- paste(directory,file_list[i],sep="")
        file_data <- read.csv(file_dir)
        # remove NA,
        file_data <- subset(file_data,complete.cases(file_data))        
        # and calculate the cor and accumulate it in the corellation vector.
        correlation <- c(correlation,cor(file_data$nitrate,file_data$sulfate))    
    }
    #Finally, return the vector
    correlation
}

# >============== TEST SCENARIOS ==============<

#source("corr.R")
#source("complete.R")
cr <- corr("specdata", 150)
head(cr)
## [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630
cr <- corr("specdata", 400)
head(cr)
## [1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630
cr <- corr("specdata", 5000)
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 
length(cr)
## [1] 0
cr <- corr("specdata")
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -1.0000 -0.0528  0.1070  0.1370  0.2780  1.0000
length(cr)
## [1] 323