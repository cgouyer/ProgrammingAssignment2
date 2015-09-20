# The following 2 functions allowed to cache the inverse of a matrix, in order to
# made some benefit in terms of costly computation (ie in order to no compute the
# of a matrix repeatedly.

# 1st function : makeCacheMatrix 
# This function creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# 2nd function : cacheSolve
# This second function returns the inverse of the matrix. 
# First, the function checks if the inverse of the matrix already exist 
# (already computed). If so, it gets the result and skips the computation. 
# If not, it computes the inverse, and sets the value in the cache via 
# setinverse function.
# Note : This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
# Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
