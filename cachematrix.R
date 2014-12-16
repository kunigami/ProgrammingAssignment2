## Computes the inverse of a matrix. It caches the result for faster 
## computation

## Constructs a class-like structure that is just a thin wrapper around 
## a matrix and can store a cached version of the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x   <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inv) i <<- inv
  
  getInverse <- function() i
  
  list(
    setInverse = setInverse,
    getInverse = getInverse,
    get = get,
    set = set
  )
}

## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
