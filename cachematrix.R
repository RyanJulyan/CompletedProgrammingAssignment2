## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function (What if I do not want to :P

# This function takes a matrix as an input and creates a list
# This function contains 4 'member' functions: set, get, setInv and getInv. 

  makeCacheMatrix <- function(x = matrix()) {

      matrixInv <- NULL # store inversion
      set <- function(y) {
	  x <<- y
	  matrixInv <<- NULL # matrixInv = null
      }

      get <- function() x # return the input matrix
      setInv <- function(inv) matrixInv <<- inv # set the inversed matrix
      getInv <- function() matrixInv # return the inversed matrix
      # return a list that contains these functions, so that we can use the functions in the object
      list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)
  }


  cacheSolve <- function(x, ...) {
      m <- x$getInv() # get the matrixInv from parameters
      if(!is.null(m)) { # if the inversion result is set and != NULL
	      message("getting cached data")
	      return(m) # return the calculated inversion
      }
      data <- x$get() # if not, we do x$get to get the matrix object
      m <- solve(data) # we solve it
      x$setInv(m) # we then set it to the object
      m # return the solved result
  }

  # Test the function
  # generate a random square
  testMatrix <- matrix(runif(4,1,100),2,2)
  testCachedMatrix <- makeCacheMatrix(testMatrix)

  testInv <- cacheSolve(testCachedMatrix)