## Pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(matrix = matrix()) {
    # store inverse value
    inverse <- NULL
    # set the original matrix and reset inverse
    set <- function(y) {
        matrix <<- y
        inverse <<- NULL
    }
    # get the original matrix
    get <- function() matrix
    # set inverse value
    set_inverse <- function(inv) inverse <<- inv
    # get inverse value
    get_inverse <- function() inverse
    
    # Returns a list of the 4 functions, this list is the special "matrix"
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(special_matrix, ...) {
    inverse <- special_matrix$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- special_matrix$get()
    inverse <- solve(data, ...)
    special_matrix$set_inverse(inverse)
    inverse
}


## Unit tests (with expected output) for Programming Assignment 2

source("cachematrix.R")

amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
amatrix$get()         # Returns original matrix
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4

cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

amatrix$get_inverse()  # Returns matrix inverse
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
# [,1] [,2]
# [1,] -0.13333333  0.2
# [2,]  0.01010101  0.0

amatrix$get()         # Returns matrix
# [,1] [,2]
# [1,]    0   99
# [2,]    5   66

amatrix$get_inverse()  # Returns matrix inverse
# [,1] [,2]
# [1,] -0.13333333  0.2
# [2,]  0.01010101  0.0

