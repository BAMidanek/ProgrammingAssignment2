## The purpose of these functions is to allow the user to cache the inverse of a matrix
## and later retrieve it from the cache rather than having to recalculate it. 
## This can be valuable because calculating the inverse of a matrix can be a large, slow computation.
## If the contents of a matrix are constant and we intend to call its inverse repeatedly, these functions
## allow us to calculate, store, and retrieve the inverse of the matrix more easily.


## The makeCacheMatrix() function creates a list containing 4 functions that are used to:
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## The cachesolve() function computes the inverse of the matrix created with the makeCacheMatrix()
## function above. Before computing the inverse, the function first checks to see if the inverse has
## already been calculated and cached. If it has already been calculated, the function retrieves the
## inverse from the cache. If it has not been computed, the function computes the inverse.


cachesolve <- function(x, ...) {
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
