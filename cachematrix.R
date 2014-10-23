#This code will find the inverse matrix. The inverse matrix will only
#be computed if that has not been done before


#makeCacheMatrix contains four functions
#the "set" function saves the input 'y' in 'x'
#the "get" function returns 'x'
#the "setmatrix" function invertes the matrix 'm'
#the "getmatrix" function returns the matrix m
#the return of "makeCacheMatrix" is a list containing the output form set, get, setmatrix & getmatrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


#the cacheSolve function returns the inverse matrix of x
#it assigns the "getmatrix" output from "Makecachematrix" to "m"
#if "m" is NOT empy => returns the inverse matrix which is cached
#else it solves the matrix for det first time and saves it in the cache
#finally it returns the inverted matrix

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
