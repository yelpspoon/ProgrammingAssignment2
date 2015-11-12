## Caching the Inverse of a Matrix

# `makeCacheMatrix`: This function creates a special "matrix" object
#  that can cache its inverse.

makeCacheMatrix <- function(gma = matrix()) {
    inv_ma <- NULL
    set <- function(ma) {
      gma    <<- ma
      inv_ma <<- NULL
    }
    get <- function() gma
    setinv <- function(invma) inv_ma <<- invma
    getinv <- function() inv_ma
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# `cacheSolve`: This function computes the inverse of the special
#  "matrix" returned by `makeCacheMatrix` above. If the inverse has
#  already been calculated (and the matrix has not changed), then
#  `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_ma <- x$getinv()
    if(!is.null(inv_ma)) {
      message("getting cached data")
      return(inv_ma)
    }
    data <- x$get()
    inv_ma <- solve(data, ...)
    x$setinv(inv_ma)
    inv_ma
}

## test data
#my.data <- rbind(c(3, 5), c(7, 9))
#my.ma <- makeCacheMatrix(my.data)
#cacheSolve(my.ma)
