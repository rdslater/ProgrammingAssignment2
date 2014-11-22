## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeMatrix <- function(x = matrix()) {

 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
							}
        get <- function() x  ##assigns 'get' to the value of 'x' which is the variable for makeMatrix
        setinv <- function(dummy) m <<- dummy  ##assigns 'setinv' a value of the function(dummy).  Assigns a "Global" Variable m equal to solve.
        getinv <- function() m ## simply assigns getinv the value of m
        list(set = set, 
			 get = get,
             setinv = setinv,
             getinv = getinv)  ##creates a list of 4 items.  'set', 'get', 'setinv'. 'getinv'
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }   ##tests if m is null and if it is NOT null, returns m (which is the cached value)
        else{data <- x$get()  ##m is null, so run the solve function
        m <- solve(data, ...)
        x$setinv(m)
        m} 
}

