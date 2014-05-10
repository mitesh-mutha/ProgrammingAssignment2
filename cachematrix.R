## makeCacheMatrix() makes a special kind of matrix which can
## be used to cache the inverse results for the same.

makeCacheMatrix <- function(x = matrix()) {
        ## We initially set the inverse result, m, as NULL
        m <- NULL
        
        ## Whenever there is a set function called on the matrix and matrix is changed,
        ## we may have to recompute the inverse. Hence, we set it to NULL. 
        ## Basically, we are clearing the cache.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Return the matrix
        get <- function() x
        
        ## Set the inverse of the matrix. Notice the "<<-"
        setinverse <- function(solve) m <<- solve
        
        ## Retrieve the inverse result.
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve() retrieves the inverse if it already exists in the cache
## or it comptes the same. A message "getting cached data" is printed
## when the result is not computed but obtained from cache.

cacheSolve <- function(x, ...) {
        
        ## Retrieves the inverse result for the matrix ( may be NULL )
        m <- as.list(x)$getinverse()
        
        
        if(!is.null(m)) {
                ## If not NULL, we have computed this earlier and
                ## result is in the cache
                message("getting cached data")
                return(m)
        }
        
        ## If NULL, we have to compute the result. We obtain the matrix first and then,
        ## compute the inverse result
        data <- x$get()
        m <- solve(data, ...)
        
        ## The following statement sets the inverse. [[3]] --- setinverse
        x[[3]](m)
        
        m
}
