## These two functions will 
## first, make a list of functions to invert a matrix 
## and cache the inverted matrix 
## second, examine the cached matrix functions for the inverted matrix
## and calculate (if not cached) then return the inverted matrix 

## The first function makeCacheMatrix creates a list of functions that
## first, set the value of the matrix within the function
## second, return the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set.mat <- function(y){
                y<<-x
                inv<<-NULL
        } 
        get.mat <- function() x
        set.inv <- function(solve) inv <<- solve
        get.inv <- function() inv
        list(set.mat=set.mat,
             get.mat=get.mat,
             set.inv=set.inv,
             get.inv=get.inv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$get.inv()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        fresh <- x$get.mat()
        inv <- solve(fresh, ...)
        x$set.inv(inv)
        inv
}
