## makeCacheMatrix is a function that creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 	inv<-NULL
	set<-function(y){
  	x<<-y
  	inv<<-NULL
	}
	get<-function() x
	setinverse<-function(solve) inv<<- solve
	getinverse<-function() inv
	list(set=set, get=get,
   		setinverse=setinverse,
   		getinverse=getinverse)

}


## cacheSolve is a function that computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getinverse()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    matrix<-x$get()
    inv<-solve(matrix, ...)
    x$setinverse(inv)
    inv
}
