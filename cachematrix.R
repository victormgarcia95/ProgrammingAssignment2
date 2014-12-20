## The follwing functions obtain the Inverse of a Sqaure Matrix
##
##
## makeCacheMatrix - This function creates a matrix object that can cache the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x} 
        setinv <- function(inversa) {inv <<- inversa}
        getinv <- function() {inv}
        list(set = set, get = get, setinv = setinv, getinv = getinv)        
}


## cacheSolve - This function calulates the inverse of the matrix returned by 
## the function makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv) 
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv                
}
