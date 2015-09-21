makeCacheMatrix <- function(x = matrix) {
        ## set variables to outside the local environment
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## create a list of function calls
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' and cache the value
        
        m <- x$getInverse()
        
        ## check to see if Inverse has been set
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## calculate and return values
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

