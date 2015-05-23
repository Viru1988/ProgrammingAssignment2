## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # Caching the Inverse of a Matrix.
        #
        # Args:
        #   x: A Matrix of nrows and nColumns.
        # 
        #   verbose: If TRUE, prints sample covariance; if not, not. Default is TRUE.
        #
        # Returns:
        #   A list is returned with get,set,getinverse,setinverse
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {x}
        setinverse <- function(inv) {m <<- inv}
        getinverse <- function(){m} 
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}



## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
        # calculating  inverse for the cached matrix from above method.
        #
        # Args:
        #   x: A Matrix of nrows and nColumns.
        # 
        #   verbose: If TRUE, prints sample covariance; if not, not. Default is TRUE.
        #
        # Returns:
        #   A list is returned with get,set,getinverse,setinverse
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        m <- x$get()
        i <- solve(m, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}

