##
## Matrix inversion is usually a costly computation, so caching becomes
## a useful mechanism to avoid re-computing the inverse of the same matrix.
## This file defines two functions for this purpose.
##

## makeCacheMatrix creates a "special" matrix that simply allows to cache its 
## inverse upon first computation. The given matrix x is assumed to be invertible.
## To solve the matrix use the cacheSolve function applied on the returned value
## from this function.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve returns the inverse of a given matrix x, where
## x was built with the makeCacheMatrix function.
## NOTE: we add an optional parameter 'getinv' for the function to be used
## for the actual inversion, which is 'solve' by default. What this
## in particulur allows is to run unit tests that verify the function
## is actually called only once upon multiple calls to cacheSolve on the
## same matrix.  See unit test at the end of this file.
cacheSolve <- function(x, getinv = solve, ...) {
  inverse <- x$getinverse()
  if(is.null(inverse)) {
    fun <- match.fun(getinv)  # match.fun to "extract the desired function objec"
    inverse <- fun(x$get(), ...)
    x$setinverse(inverse)
  }
  #else message("inverse gotten from cache")
  
  inverse
}

######################################################################################
## Not requested by the assignment, but what follows are unit tests to verify
## the correctness of the above functions.
## These unit tests can be run with RUnit. If not already on your system, you can
## install RUnit:
##    > install.packages("RUnit")
##    > library(RUnit)
##
## then run the unit tests (assuming this file is in your current directory):
##    > runTestFile("cachematrix.R")
##
## which should generate output similar to:
##
##    Executing test function test.basic  ...  done successfully.
##    Executing test function test.cache  ...  done successfully.
##    
##    Number of test functions: 2 
##    Number of errors: 0 
##    Number of failures: 0 


# test.basic tests that cacheSolve(makeCacheMatrix(m)) is equals to solve(m)
# for an invertible matrix m.
test.basic <- function() {
  m <- matrix(c(1,4,2,8,5,3,9,9,0), 3, 3)
  directInverse <- solve(m)  # for the comparison below
  
  cmatrix <- makeCacheMatrix(m)
  checkEquals(directInverse, cacheSolve(cmatrix))
}

# test.cache tests that the function to actually compute the inverse is 
# only called once upon multiple calls to cacheSolve on the same matrix
test.cache <- function() {
  
  # instead of using the default 'solve' function, we will pass cacheSolve a new 
  # function that simply wraps 'solve' while also counting the number of times it 
  # is called. This will allow to verify that 'solve' is actually called only once.
  callsToSolve <- 0
  mySolve <- function(x, ...) {
    callsToSolve <<- callsToSolve + 1
    solve(x, ...)
  }
  
  m <- matrix(c(1,4,2,8,5,3,9,9,0), 3, 3)
  directInverse <- solve(m)  # for the comparisons below
  cmatrix <- makeCacheMatrix(m)
  
  checkEquals(0, callsToSolve)

  # do multiple calls to cacheSolve while verifying that the inverse is as expected
  # and also that the inversion function was called only once:
  checkEquals(directInverse, cacheSolve(cmatrix, getinv = mySolve))
  checkEquals(1, callsToSolve)
  checkEquals(directInverse, cacheSolve(cmatrix, getinv = mySolve))
  checkEquals(1, callsToSolve)
  checkEquals(directInverse, cacheSolve(cmatrix, getinv = mySolve))
  checkEquals(1, callsToSolve)
}

