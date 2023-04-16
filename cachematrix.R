## Put comments here that give an overall description of what your
## functions do
## My two functions store a matrix and its inverse,
## and if its inverse wasn't calculated before,
## calculate and return this.

## Write a short comment describing this function
## My first function recieves a matrix "x", 
## and generate a list that contains 4 functions.
## 1st sets variable x to matrix that this function recieves
## and resets variable i.
## 2nd return the matrix stored
## 3rd set variable i to the inverse of matrix stored
## 4th return the inverse stored

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y) {
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinv<-function(solve) i<<-solve
  getinv<-function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## This function return existing inverse matrix if possible
## If there isn't inverse in the list,
## then caluculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data, ...)
  x$setinv(i)
  i
}
