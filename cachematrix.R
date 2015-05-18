## Write a function in R that is able to cache the inverse of a matrix
   
##The function makeCacheMatrix creates a special matrix function 
##that can cache its inverse and return a special "matrix".

  
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y)  {
         x <<- y
         m <<- NULL
    }
    get <-function() x
    setmatrix <- function(solve) m<<-solve
    getmatrix <- function() m
    list( set=set,get=get,
          setmatrix=setmatrix,getmatrix=getmatrix )
}
##The function cacheSolve takes the special "matrix" returned 
## by MakeCacheMatrix() above and computes its inverse.
## If the inverse already exists and is unchanged then  
## cacheSolve retrieves it from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getmatrix()
  if(!is.null(inv)) {
      message("getting cached data") ## retrieves inverse from cache
      return(inv)
  }
  data <-x$get()
  inv <- solve(data,...)  ## computes the inverse
  x$setmatrix(inv) ## caches the inverse that has been computed 
  return(inv)
}
