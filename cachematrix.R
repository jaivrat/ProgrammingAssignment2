## Two functions
## 1. makeCacheMatrix : Creates special Matrix. This matrix caches its inverse
## 2. cacheSolve      : This function computes the inverse of the special "matrix" 
##                      returned by makeCacheMatrix
## Usage:
## > set.seed(1)
## > dummym <- matrix(rnorm(9),nrow=3,ncol=3)
## > testm <- makeCacheMatrix(dummym)
## > testm$get()
## > out <- cacheSolve(testm)
## > out
## > out %*% dummym
## > dummym %*% out


#--------------------------------
# The first function, makeCacheMatrix  creates 
# a special "matrix", which is really a list 
# containing a function to
# set():  set the value of the input matrix
# get():  get the value of the input matrix
# setinv(): force set the value of the inverse
# getinv(): get the value of the inverse
#--------------------------------


makeCacheMatrix <- function(xm = matrix()) {
    
    if(is.null(xm))
        stop("NULL input")
        
    if(class(xm) != "matrix")
        stop("input is not a matrix")
    
    if( nrow(xm) <= 0 || (nrow(xm) != ncol(xm)))
       stop("input matrix has invalid size")

    mres <- NULL
    
    set <- function(ym) {
        if(class(ym) != "matrix")
            stop("input is not matrix")
        
        if(!(all.equal(xm, ym, tolerance=0.0) == TRUE))
        {
            xm <<- ym
            mres <<- NULL
        } 
    }
    get <- function() xm
    setinv <- function(ininv) mres <<- ininv
    getinv <- function() mres
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been 
## calculated (and the matrix has not changed, ie 
## if it is not set using a different matrix)
## then the cachesolve should retrieve the #
## inverse from the cache.

cacheSolve <- function(xm, ...) {
    mres <- xm$getinv()
    if(!is.null(mres)) {
        message("getting cached data")
        return(mres)
    }
    
    data <- xm$get()
    
    if(is.null(data))
        stop("NULL input")
        
    if(class(data) != "matrix")
        stop("invalid input. matrix expected")
    
    if(nrow(data) != ncol(data) || nrow(data) == 0)
        stop("input matrix has invalid size")
        
    mres <- solve(data, ...)
    xm$setinv(mres)
    mres
}

