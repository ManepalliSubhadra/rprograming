## Below are a pair of functions that are used to create a special object that  stores a matrix and caches its inverse.
makeCacheMatrix <- function(i = matrix()) {
  inverse1 <- NULL
  set <- function(j) {
    i <<- j
    inverse1 <<- NULL
  }
  get <- function() i
  setInverse <- function(inverse) inverse1 <<- inverse
  getInverse <- function() inverse1
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



cacheSolve <- function(i, ...) {
  ## Return a matrix that is the inverse of 'i'
  inverse <- i$getInverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  mat <- i$get()
  inverse <- solve(mat, ...)
  i$setInverse(inverse)
  inverse
}


my_matrix <- makeCacheMatrix(matrix(c(4,2,7,6), 2, 2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
my_matrix$getInverse()
