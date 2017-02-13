## The script 'cachematrix' provides an example of lexical scoping
## by demonstrating R's ability to pass elements between the global
## parent environment of the script and the local environments of 
## the individual functions included within the script.

## The first Function 'makeCacheMatrix' defines 4 utility functions that
## handle storing and retrieving data from the cache, for both the matrix 
## that was provided as input, and its inverse, which is calculated.
## 'setmatrix'  stores the input matrix in the cache.
## 'getmatrix'  retrieves the input matrix from the cache.
## 'setinverse' stores the calculated inverse in the cache. 
## 'getmatrix'  retrieves the the inverse, from the cache.
## The inverse of the matrix is calculated by using a canned function
## from the R library called, 'solve()'.

mycache <- NULL
olddata <- NULL

makeCacheMatrix <- function(mydata = matrix()) {
    setmatrix <- function(y) {
        mydata <<- y
        mycache <<- NULL
    }
    getmatrix <- function() mydata
    setinverse <- function(solve) mycache <<- solve
    getinverse <- function() mycache
    list(setmatrix = setmatrix, getmatrix = getmatrix, 
        setinverse = setinverse, 
        getinverse = getinverse)
}

## The second Function 'cacheSolve' checks to see whether it
## already has values stored in the cache, AND whether the input matrix's 
## values have changed from what was stored at last execution. 
## If the matrix values have changed, then the inverse already stored 
## in the cache, is no longer correct for the purpose, as it no longer 
## matches the matrix currently being provided as input.
## If it does already have the correct result stored in the cache, then 
## it skips calculating a new inverse, and simply retrieves it from the 
## cache and displays it at the console.
## If it does not have the correct results stored in the cache, or it does
## not have any values in the cache at all, then it calculates a new inverse, 
## stores the brand new inverse results in the cache, and stores the new matrix 
## values just acquired, in a global variable that can be accessed again at 
## the next successive execution to determine whether the input matrix has 
## changed again or not.  Finally, it displays the results at the console.       
        
cacheSolve <- function(mydata, ...){
    mycache <- mydata$getinverse()
    data <- mydata$getmatrix()    
    if(!is.null(mycache) && (identical(olddata,data))) {
        message("getting cached data")
        return(mycache)
    }
    mycache <- solve(data, ...)
    mydata$setinverse(mycache)
    olddata <<- mydata$getmatrix()    
    mycache
}
