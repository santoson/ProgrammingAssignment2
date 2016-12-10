## Put comments here that give an overall description of what your
## functions do
## R Programming Assignment 2
## Creates R functions that is able to cache the value of a matrix inverse

## Write a short comment describing this function
## Creates a special matrix which contains functions to
## 1. Set the matrix value
## 2. Get the matrix value
## 3. Set the value of the inverse
## 4. Get the value of the inverse
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


## Write a short comment describing this function
## cacheSolve returns the inverse of x using the cached value 
## in case cached value is null, find the inverse and cached the result
## and returns the inverse result
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
