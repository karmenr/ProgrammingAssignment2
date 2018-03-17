## Function makes a inverse matrix. If the inversed matrix doesn't have values, 
## it gives values and caches them and when it does, 
## it takes values from cache.

## Function takes in matrix, sets it. Gets the matrix,
## sets it inverse and caches it's values.
## This is the input of cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Function gets it's input from makeCacheMatrix and calculates it's inverse.
## If the inverse is already calculated, it fethces the data from cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
