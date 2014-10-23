# --------------------------------------------------------------
# Creates a special matrix object that can cache its inverse.
# Returns a list containing functions to:
# -- set the value of the matrix
# -- get the value of the matrix
# -- set the value of the matrix inverse
# -- get the value of the matrix inverse
# --------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# --------------------------------------------------------------
# Computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then cacheSolve 
# should retrieve the inverse from the cache.
# --------------------------------------------------------------
cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
      message("Getting cached data.")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
