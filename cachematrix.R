## Functions to cache the inverse of a matrix and return it when required.

## The below function sets the original matrix, gets the original matrix, sets the inverse matrix and gets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  ## Setting the original matrix in the first run
  set_original <- function(y){
    x <<- y
    inverse_matrix <<- NULL
  }
  ## Getting the original matrix
  get_original <- function() {
    x
  }
  ## Setting the value of inverse matrix
  set_inverse <- function(inv) {
    inverse_matrix <<- inv
  }
  ## Getting the value of inverse matrix
  get_inverse <- function() {
    inverse_matrix
  }
  ## Return a list that contains the functions set/ get original, set/ get inverse
  list(set_original = set_original, get_original = get_original,
       set_inverse = set_inverse, get_inverse = get_inverse)
}


## Function to get the inverse of a matrix if already calculated else to calculate the
## inverse and set it in cache for the first time

cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if(!is.null(inv)){
    message("Getting cached inverse")
    return(inv)
  }
  #if inverse does not exist, then it is calculated first and then set in cache
  temp_matrix <- x$get_original()
  inv <- solve(temp_matrix, ...)
  x$set_inverse(inv)
  inv
}

## Sample run:
## a <- matrix(1:4, nrow = 2, ncol = 2)
## b = makeCacheMatrix(a)
## b$get_original()
## In the first run, there is no inverse present in cache so the inverse is calcualted and set
## cacheSolve(b)
## Retrieving from the inverse matrix from cache in the second run
## cacheSolve(b)