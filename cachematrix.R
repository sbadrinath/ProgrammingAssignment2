# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
 # initialize to NULL
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Return the matrix
  get <- function() x
  # setmatrix function store the inverted matrix (which is passed as argument) in cache
  setmatrix <- function(invmatrix) m <<- invmatrix
  # getmatrix function returns the inverted matrix stored in cache
  getmatrix <- function() m
  # return the created functions
  list(set = set, get = get, setinvmatrix = setmatrix, getinvmatrix = getmatrix)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## get the inverse matrix stored in cache
  m <- x$getinvmatrix()
  # check if gotten value in the above line is NOT null
  # else create the matrix 
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("Data not in Cache. Hence inversing the matrix")
  data <- x$get()
  # Inverse the matrix
  m<-solve(data, ...)
  # Preserve the inverted matrix in Cache
  x$setinvmatrix(m)
  # return matrix
  m
}
