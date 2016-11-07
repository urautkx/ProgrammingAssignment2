
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inv<-NULL
  
  get <- function() x
  
  set <- function(y) {
      x<<-y
      inv<<-NULL
  }
  
  getInverse <-function()inv
  
  setInverse <-function(invM)inv <<-invM
  
  list ( get = get,
         set = set,
         getInverse = getInverse, 
         setInverse = setInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invM<-x$getInverse()
  if(!is.null(invM)) {
      message("getting cached inverse")
      return(invM)
  }
  y<-x$get()
  invM<-solve(y)
  x$setInverse(invM)
  invM
  
}
