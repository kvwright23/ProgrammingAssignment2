# Example usage of these functions:
# > x <- matrix(rnorm(25), nrow = 5)          Creates a matrix x
# > cm <- makeCacheMatrix(x)                  Creates a special matrix
# > cm$get()                                  Returns the matrix
# > cacheSolve(cm)                            Returns the inverse of the matrix
# > cacheSolve(cm)                            Call function twice to return cache

# makeCacheMatrix: returns a list of functions that will:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# cacheSolve: Computes the inverse of a matrix. If the inverse is already
# calculated, it returns the cached matrix.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
