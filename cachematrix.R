## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse. 
## makeCacheMatrix returns a list of four functions: set, get, setinverse, and getinverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## set allows the user to set the value of the matrix. Note the use of the <<- operator
        ## so x and inv are still defined later outside of the makeCacheMatrix function
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get allows the user to retrieve the value of the matrix.
        get <- function() x
        ## setinverse is used to set the value of the inverse of the matrix.
        setinverse <- function(inverse) inv <<- inverse
        ## getinverse allows the user to retreive the value of the inverse of the matrix.
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. Note that makeCacheMatrix returns a list, so cacheSolve needs such a list as its input.
## However, if the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve prints "getting cached data" and retrieves the cached version of the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'. 
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
