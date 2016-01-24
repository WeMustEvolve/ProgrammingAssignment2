## makeCacheMatrix() and cacheSolve() functions below will cache
## and compute the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that
## can cache its inverse

makeCacheMatrix <- function(matrx = matrix()) {
        inverse <- NULL
        set <- function(y) {
                matrx <<- y;
                inverse <<- NULL;
        }
        get <- function() return(matrx);
        setinverse <- funtion(inv) inverse <<- inv;
        getinverse <- funtion() return(inverse);
        return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}


## cacheSolve: This function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(matrx, ...) {
        inverse <- matrx$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- matrx$get()
        inverse <- solve(data, ...)
        matrx$setinverse(inverse)
        return(inverse)
}
