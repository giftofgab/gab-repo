## This function creates a list that provides a cache of the inverse values

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(mean) m <<- mean
        getinverse <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


## This function computes the inverse values that are stored in the cache of the above function. If the inverse has been calculated and stored in the cache, the result will be retrieved from the cache instead of being computed again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
                else}{
                        data <- x$get()
                        inverse <- solve(data, ...)
                        x$setinverse(inverse)
                        inverse
                }
}

