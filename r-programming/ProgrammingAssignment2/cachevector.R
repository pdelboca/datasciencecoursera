makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

# >============ EXAMPLES ==============<

a <- makeVector(c(1,2,3,4)) # Creates a list
a$get()
#[1] 1 2 3 4 
a$getmean()
#NULL
cachemean(a) #calculates and set the mean of the Vector "a"
#[1] 2.5
a$getmean()  # this is only to show you that the mean has been stored and does not affect anything
#[1] 2.5
cachemean(a) # as the mean has been calculated previously, this returns the saved one
#[1] 2.5
a$set(c(10,20,30,40)) # Set overrides x and set m <- NULL
a$getmean()
#NULL
cachemean(a)
#[1] 25
cachemean(a)
#getting cached data
#[1] 25
a$get()
#[1] 10 20 30 40
a$setmean(0)  # do NOT call setmean() directly despite it being accessible for the reason you will see next
a$getmean()
#[1] 0      # obviously non-sense since...
a$get()
#[1] 10 20 30 40
cachemean(a)
#[1] 0    # as you can see the call to setmean() effectively corrupted the functioning of the code
a <- makeVector(c(5, 25, 125, 625))
a$get()
#[1] 5 25 125 625
cachemean(a)
#[1] 195
cachemean(a)
#getting cached data
#[1] 195