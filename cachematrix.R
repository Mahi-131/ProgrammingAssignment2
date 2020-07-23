## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##It will create a matrix according to our requirement
makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<- NULLL
  }
  get<-function(){x}
  setInverse<-function(inverse){inv<<-inverse}
  getInverse<-function() {inv}
  list(set=set,get=get, setInverse=setInverse, getInverse=getInverse)
}
##It will check whether matrix is inversed or not and it will cache the matrix
cacheSolve<-function(x,...){
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}
