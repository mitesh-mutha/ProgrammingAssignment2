## makeCacheMatrix() makes a special kind of matrix which can
## be used to cache the inverse results for the same.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve() retrieves the inverse if it already exists in the cache
## or it comptes the same. A message "getting cached data" is printed
## when the result is not computed but obtained from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- as.list(x)$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x[[3]](m)
        m
}
