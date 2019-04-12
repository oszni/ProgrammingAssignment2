# Creation of the function to cache a matrix (argument to be passed is a matrix e.g. MyMatrix)
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

# Creation of the function to calculate the inverse of matrix or retrieve it from the cache if already calcualted
# (argument to be passed is the result of makeCacheMatrix)
cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
# Code verification on a few cases
# Verification 1
m1<-makeCacheMatrix(matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2))
n1<-cacheSolve(m1)
n1 # computed for the first run
n1<-cacheSolve(m1)
n1 # obtained from the cache

# Verification 2
m2<-makeCacheMatrix(matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2))
n2<-cacheSolve(m2)
n2 # computed for the first run
n2<-cacheSolve(m2)
n2 # obtained from the cache
