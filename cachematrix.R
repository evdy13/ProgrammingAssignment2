## This function will first create a matrix that can cache its inverse and return it without
## having to calculate it again.


## This creates the matrix which will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    if (identical(x,y)) return             ## to make sure that the matrix has not changed
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean) 
}


## Calculate the inverse of the matrix above, or retrieve it from cache if it has already been calculated.

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)                   
  x$setmean(m)
  m
}
