# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here).
#The function makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = numeric()) {

        cache <- NULL
        
        setMatrix <- function(newValue) {
                x <<- newValue
                cache <<- NULL
        }

        getMatrix <- function() {
                x
        }

        cacheInverse <- function(solve) {
                cache <<- solve
        }

        getInverse <- function() {
                cache
        }
        
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}
#The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(y, ...) {
        # get the cached value
        inverse <- y$getInverse()
        # if a cached value exists then return it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }

        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)

        inverse
