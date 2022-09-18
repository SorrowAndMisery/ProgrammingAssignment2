## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## In order to cache the matrix firstly we need to create special entity out of
## our initial matrix. This entity basically called object, (or class in other
## object oriented programming languages, like c++). Here we introduced object
## by function makeCasheMatrix, with number of methods, that we will use in 
## cacheSolve function. 
##
## In the casheSolve function matrix solved and stored in the object.
## Here in the line 47 actual solving the inverse occurs. and in the line 48
## we write the solved function to the object, frow which it can be get any
## time

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
    }
  
  get <- function() {
    return(x)
    }
  
  setinve <- function(inverse) {
    i <<- inverse
    }
  
  getinv <- function() {
    return(i)
    }
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
  i <- x$getinv()
        
  if(is.null(i)) {
        data <- x$get()
        i <- solve(data) %*% data
        x$setinv(i)
        return(i)
  } else {
        return(i)
          }
  
}
