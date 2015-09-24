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


## Function to solve the provided cacheMatrix
## Returns the cached solution (inverse) if available
## Otherwise calculate, store, and return the inverse of the provided matrix

## > k <- makeCacheMatrix(mxx$empty)
## > cacheSolve(k)
##      [,1]
## [1,]   NA
## > cacheSolve(k)
## [using cached solution]
##      [,1]
## [1,]   NA


cacheSolve <- function(mx, ...) {
  ## Return a matrix that is the inverse of 'mx'
  ## mx is is a matrix wrapped by makeCacheMatrix
  inv <- mx$getinverse()
  if (!is.null(inv)) {
    message("[using cached solution]")
    return(inv)
  }

  inv <- solve(mx$get(), ...)
  mx$setinverse(inv)
  inv
}


# test fixtures

mxx = list(
  empty = matrix(),
  zero = matrix(c(0)),
  one = matrix(c(1)),
  a = matrix(c(0,1,1,0),2),
  b = matrix(c(1,0,0,1),2),
  x3 = matrix(c(1,0,0, 0,2,0, 0,0,3), 3)
)

# > k <- makeCacheMatrix(mxx$x3)
# > cacheSolve(k)
#      [,1] [,2]      [,3]
# [1,]    1  0.0 0.0000000
# [2,]    0  0.5 0.0000000
# [3,]    0  0.0 0.3333333
# > cacheSolve(k)
# [using cached solution]
#      [,1] [,2]      [,3]
# [1,]    1  0.0 0.0000000
# [2,]    0  0.5 0.0000000
# [3,]    0  0.0 0.3333333
# >

## Odd thing I don't understand:
# > j <- makeCacheMatrix(mxx$one)
# > cacheSolve(j)
#      [,1]
# [1,]    1
# > cacheSolve(j)
# [using cached solution]
#      [,1]
# [1,]    1

# > k <- makeCacheMatrix()
# > cacheSolve(k)
#      [,1]
# [1,]    1

## I get a new cacheMatrix but if I don't specify 'mx' I get an uncached copy of the previous one.
