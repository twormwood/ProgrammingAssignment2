# The Make Cache Matrix function is used to define a matrix whose inverse will 
# later be stored in the cache. This function contains 4 functions: set, get, 
# setinverse, and getinverse.
# The first line of the function defines that the x argument defined in the 
# function will be in the form of a matrix. "m" is an empty variable that is 
# set to NULL and will be reset to NULL everytime this function is invoked.
# Next, the set function is defined. The matrix used for makeCacheMatrix can be 
# changed by invoking object$set(new matrix). The get function follows. Using
# object$get() returns the matrix defined by the object.
# The setinverse function stores the inverse of the matrix in the cache, and the
# getinverse function will return the matrix to the console using 
# object$getinverse().
# Finally, the different functions are assigned names so that any of the 
# four functions can be called using the $ symbol in conjunction with the 
# object.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() {x}
  setinverse<-function(solve) {m<<-solve}
  getinverse<-function() {m}
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function (cacheSolve) is what actually determines the inverse of the
# matrix defined by makeCacheMatrix. The second line retrieves the cached
# inverse if the inverse of the matrix has already been determined in a 
# previous invocation. To demonstrate that the inverse matrix was retrieved 
# from the cache, the message "getting cached data" appears in red type.
# Starting from line 7, when the inverse is not found, the matrix defined
# by makeCacheMatrix is retrieved, and then solved in line 8. Line 9 stores
# the inverse matrix in the cache and returns the inverse matrix to the command
# line. 

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setinverse(m)
  m
}
