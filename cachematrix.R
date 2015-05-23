## This is a set of functions that will cache the inverse of matrix and use it from cache
##  for the next time when it is needed. 

## This function will cache the inverse of special matrix

makeCacheMatrix <- function(x = matrix()) {
        # Caching the Inverse of a Matrix.
        #
        # Args:
        #   x: A Matrix of nrows and nColumns.
        
        m <- NULL        
        #set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #get the value of the matrix
        get <- function() {x}
        #set the value of the matrix
        setinverse <- function(inv) {m <<- inv}
        #get the value of the matrix
        getinverse <- function(){m} 
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
        # calculating  inverse of the special matrix  by reusing the cached matrix 
        # from the above method.
        #
        # Args:
        #   x: Cached Iverse of Matix.
        # 
        # Returns:
        #  Return a matrix that is the inverse of 'x'
        
        #get the inversed matrix from X and assign it to variable i
        i <- x$getinverse()
        #if retrived inverse of matrix i is not null then return it or 
        # do the inverse of matrix and return it.
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        m <- x$get()
        i <- solve(m, ...)
        x$setinverse(i)
        ## Return a matrix that is the inverse of 'x'
        return(i)        
}

