## In linear algebra, an n-by-n square matrix A is called invertible (also nonsingular or nondegenerate) if there exists an n-by-n square matrix B such that
## mathbf{AB} = \mathbf{BA} = \mathbf{I}_n \  
## where In denotes the n-by-n identity matrix and the multiplication used is ordinary matrix multiplication. If this is the case, then the matrix B is 
## uniquely determined by A and is called the inverse of A, denoted by A−1.

## Example:
## c=rbind(c(1, -1/4), c(-1/4, 1))
## m1 <- makeCacheMatrix(c)
## r1 <- cacheSolve(m1)
## r1 <- cacheSolve(m1)

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
			x <<- y
			i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {		
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")				
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
