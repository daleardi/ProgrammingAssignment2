## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## This is a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  #set inverse to NULL
        
        #function to set the matrix
        set <- function(y) {
                x <<- y   #sets value to input                
                inv <<- NULL #remove any previously stored inverse
        }
        
        #function to get the matrix
        get <- function() x
        
        #function to set the inverse of the matrix
        setinv <- function(solve) inv <<- solve
        
        #function to get the inverse of the matrix
        getinv <- function() inv
        
        #return a list of the previous functions
        list(set=set, get=get, 
             setinv=setinv, 
             getinv=getinv)     
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve will retrieve the 
## inverse from the cache.


cacheSolve <- function(x, ...) {
        inv <- x$getinv() #get the inverse of input from cache function
        
        #check to see if the inverse is cached
        #if it is let the user know it is cached
        #and return cached value
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        #if it is not cached
        data <- x$get() #get the matrix
        inv<- solve(data,...) #solve for inverse
        x$setinv(inv) #save the invers3e in the cache
        inv #return the inverse of the matrix
}
