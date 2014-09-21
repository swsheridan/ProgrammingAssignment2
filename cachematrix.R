## These functions cache the inverse of matrix, for use as an efficient alternative   
#       to repeatedly recalcualting the inverse of the same matrix
# 
## These functions were developed as an assignment for the R Programming class on Coursera.
#  Structure, logic and some comment text in this code is derived from examples
#  provided in class.
#
## Coded by Sally Sheridan, Sept. 2014

## makeCacheMatrix function creates a special matrix object that can cache its inverse.
#        It defines functions to set and get the matrix and to set and get the inverse
#        matrix by taking advantage of super assignments to variables in parent environment.
#
#  Passed arguments x and y are square matrices that are invertible. 
#  Per assignment instructions it is assumed that the matrix supplied is always invertible.
#
#  Returns a list of the names of the associated functions to work with the cache.

makeCacheMatrix <- function(x = matrix()) {
        # Start with null inverse to indicate it has no content yet
        invmatrix <- NULL
        
        # Function to set value of persistent internal matrix x 
        set <- function(y){
                x <<- y
                invmatrix <<- NULL
        }
        
        # Function to get value of matrix x
        get <- function() x
        
        # Function to set cache with inverse of the matrix x
        setinverse <- function(i) invmatrix <<- i
        
        # Function to get cached inverse of matrix x
        getinverse <- function() invmatrix
        
        # makeCacheMatrix returns list of functions to work with cached inverse matrix
        list(set = set, get =get,
             setinverse = setinverse,
             getinverse = getinverse)       
}


## cacheSolve function serves as an altenative to the solve function
#      When the source matrix has not changed the cached inverse is returned
#      instead of recalculating the inverse.

cacheSolve <- function(x, ...) {
        # get cached value of inverse
        invmatrix <- x$getinverse()
        
        # If cached inverse is not null, then return cached inverse
        if(!is.null(invmatrix)){
                message("getting cached data")
                return(invmatrix)
        }
        # If cached inverse is null, then calculate new inverse and set it into cache
        data <- x$get()
        invmatrix <- solve(data)           
        x$setinverse(invmatrix)
        
        #cacheSolve returns inverse matrix
        invmatrix
}