## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    cachedMatrixInverse <- NULL
    set <- function(y) {
        x <<- y
        cachedMatrixInverse <<- NULL
    }
    get <- function() x
    setMatrixInverse <- function(inv) cachedMatrixInverse <<- inv
    getMatrixInverse <- function() cachedMatrixInverse
    list(set = set, get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matrixInverse <- x$getMatrixInverse()
    if(!is.null(matrixInverse)) {
        message("found cached matrix inverse")
        return(matrixInverse)
    }
    matrix <- x$get()
    calculatedMatrixInverse <- solve(matrix)
    x$setMatrixInverse(calculatedMatrixInverse)
    calculatedMatrixInverse
}
