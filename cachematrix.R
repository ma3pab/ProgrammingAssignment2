## The following functions create a matrix vector and then either fetch the
## inverse of the matrix if it has already been calculated, otherwise it
## calculates the inverse and then caches it

## The following function creates a list of four functions which enable "getting" and 
## "setting" of a matrix and its inverse and returns this to the global environment

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


## cacheSolve function uses the output of the makeCacheMatrix function to   
## retrieve the inverse of a matrix which has already been cached, or otherwise
## calculate the inverse and store it to memory using the setInverse function above


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}

