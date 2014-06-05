## Compute the inverse of a given matrix. Cache the result to avoid unnecessary computations

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Assign NULL to the variable that will store the inverse of the matrix
    inv <- NULL
  
    ## Assigns y to the matrix x and NULL to inv
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
  
    ## Returns the matrix x
    get <- function() x
    
    ## Assigns the matrix inverse to inv
    setinverse <- function(inverse) inv <<- inverse
    
    ## Returns the matrix inverse
    getinverse <- function() inv
    
    ## Return the list of functions from this function
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    ## Return the cached inverse if it is still valid
    if (!is.null(inv)) {
        message("getting the cached data")
        return(inv)
    }
    
    ## Get the matrix to be inverted
    data <- x$get()
    
    ## Invert the matrix
    inv <- solve(data, ...)
    
    ## Write the inverse of the matrix to the cache
    x$setinverse(inv)
    
    ## Return the inverse of the matrix
    inv
}
