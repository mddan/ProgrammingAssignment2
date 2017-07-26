## Below are two functions that are used to create a special object that stores 
## a matrix and cache's its inverse.

##This first function, makeCacheMatrix creates a list containing functions to:

## 1. set the matrix
## 2. get the matrix
## 3. set the inverse (solution) of the matrix
## 4. get the inverse (solution) of the matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) s <<- inverse
    getinverse <- function() s
    list (set = set, get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)
}


## The following function calculates the inverse of the special "vector" 
## created with the above function. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the matrix 
## and sets inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...){
    s <- x$getinverse()
    if(!is.null(s)){
        message("getting cached data'")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
}


