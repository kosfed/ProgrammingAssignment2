## Function inverses matrix and keeps it cached

## Returns cacheable matrix, similar to class in OOP :)

makeCacheMatrix <- function(x = matrix()) {
  ## Inner cache variable
  inversed <- NULL
  ## Set original matrix and clear old cached
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  ## Returns original matrix
  get <- function() x
  ## Calculates inversed matrix and stores it in the cache
  setinversed <- function(solve) inversed <<- solve
  ## Returns cached inversed matrix
  getinversed <- function() inversed
  ## Create callable list of functions
  list(set = set, get = get,
       setinversed = setinversed,
       getinversed = getinversed)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Tryes to get cached matrix
  inversed <- x$getinversed()
  if(!is.null(inversed)) {
    message("getting cached data")
    return(inversed)
  }
  ## If cached value isn't available calculate, cache and return it
  data <- x$get()
  inversed <- solve(data, ...)
  x$setinversed(inversed)
  inversed
}
