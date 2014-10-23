## This function takes in a value and assigns it to a matrix then uses solve to calculate inverse then make/get
## to cache it. We then use another function to see if we have cached value if not we use solve to calculate the iverse and output it. 
## Much of the code you see below was modeled after examples given by Professor Peng

## Taking in value as a matrix and use solve function to get inverse then cache by make and get.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  make <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  makematrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(make = make, get = get,
       makematrix = makematrix,
       getmatrix = getmatrix)
  }


## Function checks if m has been calculated and we have our matrix cached already. If it's not null then we get it to save processing
## otherwise we solve it. 

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  output<- x$get()
  m <- solve(output, ...)
  x$makematrix(m)
  m
}
