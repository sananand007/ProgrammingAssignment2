## Function to show the importance of caching

# use `<<-` to assign a value to an object in an environment 
# different from the current environment.

## This first function creates a special matrix , which is a list
# 1. set the value of the Matrix
# 2. get the value of the Matrix
# 3. set the values of the Inversion of the Matrix
# 4. get the values of the Inversion of the Matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  get<-function() x
  setinv<-function(inverse) inv<<-inverse
  getinv<-function() inv
  list(set=set, get=get, setinv = setinv, getinv=getinv)
}


## The following function calculates the inverse of the special "matrix" created by the above function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv)){
          message("Getting cached data")
          return(inv)
        }
        data<-x$get()
        inv<-solve(data, ...)
        x$setinv(inv)
        inv
}

## Testing the Functions above using the function below

# using the input as a invertible matrix


# testval <- function(mat) {
#   temp <- makeCacheMatrix(mat)
#   
#   # just normal
#   starttime<-Sys.time()
#   cacheSolve(temp)
#   dur <- Sys.time() - starttime
#   print(dur)
#   
#   # cached
#   starttime<-Sys.time()
#   cacheSolve(temp)
#   dur <- Sys.time() - starttime
#   print(dur)
# }