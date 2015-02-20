## These functions allow caching the inverse of a matrix, avoiding
### repeated unnecessary computations
### author: gallalf
### 2015, February 19th

## The function below creates a special "matrix" object that can 
### cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }        
        get <- function() x        
        setinverse <- function(solve) inv <<- solve        
        getinverse <- function() inv
        matrix(list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function below checks if the inverse has already been calculated.
### If so, just returns it. Else, it calculates it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
