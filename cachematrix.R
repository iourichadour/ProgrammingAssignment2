###########################################
# Iouri Chadour - 03/22/2015
# Programming Assignment 2
# Course: R Programming - rprog-012
###########################################

######################################################################################
## This function creates a special "matrix" object that can cache its inverse.
######################################################################################
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        #cache the matrix to be used for comparison later
        setPrev <- function(y){
                prevMat <<- y
        }
        
        #get cached matrix for comparison
        getPrev <- function() prevMat

        set <- function(y) {
                x <<- y
                
                m <<- NULL
        }
        
        get <- function() x

        setInverse <- function(solve) m <<- solve
        
        getInverse <- function() m
        
        # if the matrix is not set yet - set the previous matrix too
        if( is.null(m) ){
                setPrev(x)
        }
        
        list(set = set, get = get,setPrev = setPrev, getPrev = getPrev,
             setInverse = setInverse,
             getInverse = getInverse)

}


######################################################################################
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
# should retrieve the inverse from the cache.
######################################################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        #get new and previous data
        dataNew <- x$get()
        dataCache <<- x$getPrev()
        
        #get the cached invers
        m <- x$getInverse()
        
        
        #cat(identical(dataNew, dataCache ))
        
        # check if the value is cached and the matrices are identical 
        # then return cached data, otherwise proceed
        if(!is.null(m) & identical(dataNew, dataCache ) ) {
                message("getting cached data")
                return(m)
        }
        
        # if the matrix is new or has changed proceed to recalculating the inverse
        m <- solve(dataNew, ...)
        
        #cache the Inverse
        x$setInverse(m)
        
        #cache the new Matrix for comparison
        x$setPrev(dataNew)
        
        m
        
}
