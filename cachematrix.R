## These functions take a user defined matrix and calculate the inverse of that
## matrix. The matrix inverse is cached during the first calculation so that it
## can be called when required.

## Clear all existing variables in the environment
rm(list=ls())

## This function is a function factory which takes a matrix as an argument,
## specifies the inverse as NULL.
## The function then provides methods to: 
##                      set specifies a new matrix
##                      get returns the orginial matrix input
##                      setinverse calculates the inverse of the matrix and sets
##                      the value of the inverse in the calling environment
##                      getinverse returns the calculated inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function() inverse <<- solve(x)
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function gets the inverse of a given matrix object. If the inverse does
## not exist, the inverse is calculated, otherwise the calculated inverse is 
## returned.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Get the inverse from matrix x
        inverse <- x$getinverse()
        ## Check to see if the inverse matrix has been calculated and returns 
        ## the inverse if true
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        ## If the inverse has not been calculated - it is calculated and the
        ## inverse is returned and printed.
        else {
                x$setinverse()
                inverse <- x$getinverse()  
        }
        ## Return the calculated or cached inverse
        inverse
}

## TEST SECTION: Uncomment to check that the function is working correctly
## Sets the invertable matrix
## mymatrix = makeCacheMatrix(matrix(c(5,6,7,8), nrow=2, ncol=2))
## Prints the original matrix
## mymatrix$get()
## Solves and caches the inverse
## cacheSolve(mymatrix)
## Gets and displays the inverse matrix
## mymatrix$getinverse()
## Checks that the chached inverse is obtained when inverse is not NULL
## cacheSolve(mymatrix)
