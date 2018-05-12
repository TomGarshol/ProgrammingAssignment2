

## This is a function that caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) 
        {
        inv <- NULL
        set <- function(y) 
                {
                x <<- y
                inv <<- NULL
                }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        }
        


## This function takes the matrix and computes the inverse. If the inverse is already there,
## then it should be used instead of doing a new calculation.

cacheSolve <- function(x, ...) 
        {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        
}

