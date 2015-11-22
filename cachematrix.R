# The function takes a matrix and defines up 3 functions 
# to work on it in the following cacheSolve(). They are returned in a list.

# It works by using:
# matrix <- #our matrix#
# a <- makeCacheMatrix(matrix)
# cacheSolve(a)

# The function is adapted from the example.
# It assumes that the matrix can be inversed.


makeCacheMatrix <- function(x = matrix()) { 
  i <- NULL
  get <- function() {x}
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() {i}
  list(get = get, setInverse = setInverse, getInverse = getInverse)
}



# Takes the previous function, checks if i has already been computed and 
# if not, computes the inversion. In both cases, the inverted matrix is returned.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

# Essentially it is just a rewrite of the examples 
#with solve() instead of mean(), inverse instead of mean and i insead of m. 