## This function can creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## Here I define the argument with default mode of "matrix".
    inv <- NULL                             ## This is the initialization of inv as NULL. 
                                            ## it will hold value of matrix inverse. 
    
    set <- function(y) {                    ## Here I define the set function to assign new.
        x <<- y                             ## Value of matrix in parent environment.
        inv <<- NULL                        ## If there is a new matrix, reset inv to NULL.
    }
    get <- function() x                     ## Here I define the get function; it returns the value of the matrix argument.

    setinverse <- function(inverse) inv <<- inverse  ## It assigns value of inv in parent environment.
    getinverse <- function() inv                     ## It gets the value of inv where called.
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## This is used to refer to the functions with the $ operator.
}

## This function computes the inverse of the matrix returned by makeCacheMatrix, and if the inverse is already been calculated and the matrix has not changed values,
## then cacheSolve will recover the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## This function return a matrix that is the inverse of x

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
