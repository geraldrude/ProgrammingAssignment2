## These functions allow us to save, otherwise known as cache, the
## the results of a computationally intensive matrix inversion.
## As long as the matrix has not changed, these functions will
## allow us to save the solution to the inversion and not recompute
## it every time it is accessed. If the matrix is changed, the 
## the inversion is reset to NULL and a new inversion computation
## will be performed replacing the NULL value.

## makeCacheMatrix is a function that creates a list object. Each
## element of the list contains a function to either:
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
	matrix_inv <- NULL
  set <- function(nmat) {
    x <<- nmat
    matrix_inv <<- NULL
  }
	set_inverse <- function(inverse){
    matrix_inv <<- inverse
	}
  get <- function() {
    x
	}  
  get_inverse <- function() {
    matrix_inv
  }
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
  
}
  

## cacheSolve returns the inverse of a matrix. If the matrix has not changed, and a cached
## version of the solution is available, the cached solution is returned.  If the matrix  
## inverse is uncached, a new solution is generated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix_inv <- x$get_inverse()
  if(!is.null(matrix_inv)) {
    message("getting cached data")
    return(matrix_inv)
  }
  dmatrix <- x$get()
  matrix_inv <- solve(dmatrix)
  x$set_inverse(matrix_inv)
  return(matrix_inv)
}
