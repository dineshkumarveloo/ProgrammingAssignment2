## makeCacheMatrix(x) and cacheSolve(x) are the two functions writtern as part of this submission which take in matrix as input

## makeCacheMatrix() caches the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
  m_inverse <- NULL
  set <- function(y) {
    x <<- y
    m_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m_inverse <<- inverse
  getinverse <- function() m_inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve() returns the inverse of the input matrix if it's not available, if present it accesses the cached matrix

cacheSolve <- function(x, ...) {
  m_inverse <- x$getinverse()
  if(!is.null(m_inverse)) {
    message("getting cached data")
    return(m_inverse)
  }
  m_data <- x$get()
  m_inverse <- solve(m_data, ...)
  x$setinverse(m_inverse)
  m_inverse
}
