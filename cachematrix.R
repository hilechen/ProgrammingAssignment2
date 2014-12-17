# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
              
        m <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# above. If the inverse has already been calculated (and the matrix has not changed), then the
# cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
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

# Test:
# m1 <- makeCacheMatrix()
# m1$set(matrix(c(2,2,3,2), 2, 2))
# m1$get()
# cacheSolve(m1)
# i <- cacheSolve(m1)
# i
# m1$get() %*% i


