## Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. 
## The following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix function creates a special "matrix" object that can cache the inverse of a matrix.
## This function create a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL       
     set <- function(y) {
        x <<- y    
        m <<- NULL 
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}



## The cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## then this function retrieves the inverse from the cache.


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

## *****************************************
## Sample run
##******************************************
## Sr<-makeCacheMatrix(matrix(1:4,2))
## Sr$get()
## cacheSolve(Sr) ## 1st run returns inverted matrix from working environment

## cacheSolve(Sr) ## 2nd and subsequent runs returns inverted matrix from cache


