## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.


MakeCacheMatrix <- function(x = matrix()){
        ## Initialize inverse property
        
        inv <- NULL
        
        ## Set the matrix
        set <- function(y){
                
                x <<- y
                inv <<- NULL
        }
        
        get <- function(){x}
        
        ## Set inverse of matrix
        setInverse <- function(inverse) {inv <<- iinverse}
        
        ## Get inverse of matrix
        getInverse <- function() {inv}
        
        ## Return list of methods
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

CacheSolve <- function(x, ...){
        
        ## Return a matrix that is the inverse of 'x
        inv <- x$getInverse()
        
        ## Return the inverse if it is set 
        if(!is.null(inv)){
                
                message("Getting Cached Data")
                return(inv)
        }
        
        ## Get matrix from the object
        mat <- x$get()
        
        ## Calculate inverse using multiplication
        inv <- solve(mat, ...)
        
        ## Set inverse of object
        x$selectInverse(inv)
        
        ## Return matrix
        inv
}


