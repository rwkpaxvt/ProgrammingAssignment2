## makeCacheMatrix reads in a given matrix, then creates a list with four different functions

makeCacheMatrix <- function(x = matrix()) {
## Set m to NULL whenever a new matrix is read in
  m <- NULL
  ## Define the set function as setting the given matrix and clearing any previous m value
  set <- function() {
    x <<- y
    m <<- NULL
  }
  ## Define the get function as displaying the original input matrix
  get <- function() {
    x
  }
  ## Solve in inverse of the given matrix and store it as m
  setinverse <- function(solve) {
    m <<- solve(x)
  }
  ## Get and display the inverse saved in the variable m
  getinverse <- function() {
    m
  }
  ## Store the above functions in a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function displays the inverse of x if cached.  If not, it calculates the inverse
cacheinverse <- function(x, ...) {
## this function stores the result of getinverse as m.  If not null, it displays the result 
## as calculated in the above functions stored in the cache
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## If the value of m is null, calculate the inverse of x and display it
  if(is.null(m)) {
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
  m
  }
}
