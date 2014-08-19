
## makeCacheMatrix creates a matrix decorator (a wrapped matrix object)
## that adds the ability to cache the result of solve() function which 
## finds the inverse of the matrix.
## Example:  
##    mat <- matrix(seq(1:4), 2)
##    cmat <- makeCacheMatrix(mat)
makeCacheMatrix <- function(matr = matrix()) {
  inversed <- NULL
  
  # Getter for main (non-inversed) matrix
  get <- function() matr
  
  # Setter for for inversed matrix. Pass it a result of solve() function
  set_inversed <- function(inv_matr) inversed <<- inv_matr
  
  # Getter for inversed matrix
  get_inversed <- function() inversed
  
  list(get = get,
       set_inversed = set_inversed,
       get_inversed = get_inversed)
}

## cacheSolve returns the inverse of a given matrix.
## First it looks up for result in the cache (in case if inverse was previously computed).
## If result is not found in cache it is computed by calling solve() function, stored in cache
## and then returned.
## Example:
##    cacheSolve(cmat) 
cacheSolve <- function(x, ...) {
  
  # Try to get the result from the cache
  inversed <- x$get_inversed()
  
  # If result is found then just return it
  if(!is.null(inversed)) {
    message("getting cached data")
    return(inversed)
  }
  
  # If there was nothing in the cache,
  # then get the original matrix
  data <- x$get()
  
  # Find inverse for it
  inv_matr <- solve(data)
  
  # Store the result in the cache
  x$set_inversed(inv_matr)
  
  # Return the inversed matrix
  inv_matr
}
