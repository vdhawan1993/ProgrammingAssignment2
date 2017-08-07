
## Inverse of a matrix: Vineet Dhawan
## This function helps the user to construct a special matrix for which the inverse has to 
## be calculated. If the matrix inverse has already been calculated, then the result will be 
## looked up in the cache.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)i <<-inverse
  getinverse<-function()i
  list(set=set,get=get,setinverse=setinverse, getinverse=getinverse)
}


## This function helps in finding the inverse of the special matrix created above. If the 
## inverse has already been calculated, then the result will be looked up in cache and the 
## value will be returned.

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data)
  x$setinverse(i)
  i
}
