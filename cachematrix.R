## Couple of functions to optimize inversing matrixes as it could be fairly
## computational work to find the inverse of a large matrix. Usage below:
## 
## [Inspired by Johanna Tikanmäki COMMUNITY TA usage for cachemean on
## https://class.coursera.org/rprog-014/forum/thread?thread_id=105]
##
## cMatrix <- makeCacheMatrix(m) #first a cache aware matrix where 'm' is the 
##                               #matrix you would like to use as input
## cacheSolve(cMatrix) #calculate Inverse (solve) of the matrix
## cacheSolve(cMatrix) #this will return the cached matrix inverse
## cMatrix$set(new_m)  #cMatrix can be set to a new input matrix ('new_m')
## cacheSolve(cMatrix) #calculate Inverse of new_m for the first time
## cacheSolve(cMatrix) #this will return the cached inverse
## cacheSolve(cMatrix) #same result as above; still cached


## Create a cached aware matrix that that be reused with new matrixes as well

makeCacheMatrix <- function(m = matrix()) {
    cachedMatrixInverse <- NULL       #cached matrix inverse value if set
    set <- function(y) {
        m <<- y                       #set new value for raw matrix 'm' set
                                      #at makeCacheMatrix function level using
                                      #<<-
        cachedMatrixInverse <<- NULL  #clear cached as 'm' has changed at the 
                                      #parent frame of this function
    }
    get <- function() m               #get raw matrix 'm'
                                      
    #functions below: set & get cached matrix value for makeCacheMatrix's
    #cacheMatrixInverse variable
    setMatrixInverse <- function(inv) cachedMatrixInverse <<- inv
    getMatrixInverse <- function() cachedMatrixInverse
    
    #return a list containing various functions that can be used to:
    #get raw matrix 'm'
    #set raw matrix that will clear matrix inverse as well
    #getMatrixInverse to get cachedMatrixInverse
    #setMatrixInverse to set cachedMatrixInverse
    list(set = set, get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)
}


## Solve (Inverse) and cache matrix. Has to be used as shown in the usage
## section. 'x' has to be an object returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
    matrixInverse <- x$getMatrixInverse()       #try retrieving cached value
    if(!is.null(matrixInverse)) {               #if we find the value
        message("found cached matrix inverse")  #message we have cached value
        return(matrixInverse)                   #return from this function
    }
    matrix <- x$get()                           #get actual matrix data
    calculatedMatrixInverse <- solve(matrix)    #use solve from {base} package
    x$setMatrixInverse(calculatedMatrixInverse) #set cached value for x
    ## Return a matrix that is the inverse of 'x'
    calculatedMatrixInverse
}
