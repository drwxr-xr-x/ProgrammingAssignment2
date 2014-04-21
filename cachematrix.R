#' Create a "matrix" object that is able to cache its inverse.
#' @name cachematrix
#' @author Zejun Wu \email{watashi@@users.noreply.github.com}
#' @references \url{https://github.com/rdpeng/ProgrammingAssignment2}
#' @examples
#' a <- makeCacheMatrix(matrix(1:4, 2, 2))
#' cacheSolve(a)              # compute the inverse and cache it
#' cacheSolve(a)              # get cached data
#' a$set(matrix(4:1, 2, 2))   # set the matrix
#' a$get()                    # get the matrix
#' cacheSolve(a)              # compute the inverse of the new matrix

#' Create a "matrix" object that can cache its inverse.
#' @param a a square numeric or complex matrix
#' @return a "matrix" object
makeCacheMatrix <- function(a = matrix()) {
  # Cache
  b <- NULL

  # Set the matrix
  set <- function(x) {
    a <<- x
    b <<- NULL
  }

  # Get the matrix
  get <- function() a

  # Get the inverse of the matrix
  get.inverse <- function() {
    if (is.null(b)) {
      # If the inverse has not already been calculated, then calculate and
      # cache it
      b <<- solve(a)
    }
    b
  }

  # Return the "matrix" object
  list(set = set, get = get, get.inverse = get.inverse)
}

#' Compute the inverse of the "matrix" object returned by
#' \code{\link{makeCacheMatrix}}.
#' @param a a "matrix" object
#' @param ... further arguments are ignored
#' @return the inverse of \code{a}
cacheSolve <- function(x, ...) {
  # Return the inverse
  x$get.inverse()
}
