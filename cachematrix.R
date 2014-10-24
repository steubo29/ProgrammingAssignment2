## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ##set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ##Get the matrix
        get <- function() x
        
        ##cache the inverse result
        setinverse <- function(solve) m <<- solve
        
        ##get the inverse result
        getinverse <- function() m
        
        ##List of functions available
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ##Get the inverse result cached
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ##calculate and return the inverse result cached
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
