## This function creates a special "matrix" object that can cache its inverse
## Input : invertible matrix
## Output : SPecial matrix, a list containing a function to
##          1. set the value of matrix
##          2. get the value of matrix
##          3. set the value of the inverse of the matrix
##          4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m_cache <- NULL
    set <- function(y){
      x <<- y
      m_cache <<- NULL
    }
    get <- function() x
    setinverse <- function(z) m_cache <<- solve(z)
    getinverse <- function() m_cache
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix.
## Input : special matrix returned by makeCacheMatrix
## output : Inverse of the special matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m_cache <- x$getinverse()
  if(!is.null(m_cache)) {
    message("getting cached data")
    return(m_cache)
  }
  data <- x$get()
  m_cache <- solve(data, ...)
  x$setinverse(data)
  m_cache
}
