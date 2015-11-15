## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The pair of functions cache the inverse of a matrix.

## This function makeCacheMatrix creates a special "matrix" 
## object that can cache its inverse.
## The first function, makeCacheMatrix creates a special "vector", which is 
## really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function, cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should retrieve the 
## inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## > x <- rbind(c(1, 5), c(5, 1))
## > y <- makeCacheMatrix(x)
## > y$get()
##      [,1] [,2]
## [1,]    1    5
## [2,]    5    1
## > cacheSolve(y)
##             [,1]        [,2]
## [1,] -0.04166667  0.20833333
## [2,]  0.20833333 -0.04166667
## > cacheSolve(y)
## getting cached data
##             [,1]        [,2]
## [1,] -0.04166667  0.20833333
## [2,]  0.20833333 -0.04166667