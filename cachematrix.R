## cachematrix.R
## Randall Bohn 2015-09-24
## Store a matrix and the inverse of the matrix.
## Caches the inverse so you only have to SOLVE it once.

## This is the thing that holds a matrix (mx) 
## and the inverse (inv).
## it has getters and setters for each field.

makeCacheMatrix <- function(mx = matrix()) {
  inv <- NULL
  set <- function(y) {
    mx <<- y
    inv <<- NULL
  }

  get <- function() mx
  setinverse <- function(inv_) inv <<- inv_
  getinverse <- function() inv

  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
