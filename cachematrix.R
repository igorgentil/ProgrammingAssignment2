## makeCacheMatrix cache the inverse of a matrix  in an environment 
## that is different from the current environment.
## cacheSolve use the cached matrix of the makeCacheMatrix function 
## If the the matrix wasn't calculated, 
## the inverse of the matrix is calculated and result is cached for reuse

## Calculate the inverse matrix of 'x' and cache the result 
## in an environment that is different from the current environment
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        #create 'getters and setters' of the function
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function use 'x' and verify it is cached using getter from makeCacheSolve(). 
## If the result is NULL, 'the result'x' is calculated and stored in cache for reuse.
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        
        ## Return a matrix that is the inverse of 'x' if 'x' wasn't in cache
        if(!is.null(m)) {
                message("using cached matrix")
                return(m)
        }
        
        ## Calculate the inverse of the matrix and store it in cache for reuse
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

## Example
# mt <- matrix(sample.int(15, 9*1000, TRUE), 4, 4)
# mcm <- makeCacheMatrix(mt)
# cacheSolve(mcm)
