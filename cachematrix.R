##This program illustrates the development advantage of lexical scoping through cache
##example

## makeCacheMatrix creates a matrix object and stores its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
      x<<-y
      inv<<-NULL
}
    get<-function() x
    set_inverse<-function(inverse) inv<<-inverse
    get_inverse<-function() inv
    list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
}


## cacheSolve checks whether the inverse has already been computed else it computes it
## and cache it for future query.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$get_inverse()
  if(!is.null(inv)){
    message("getting chached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(x)
  x$set_inverse(inv)
  inv
}
