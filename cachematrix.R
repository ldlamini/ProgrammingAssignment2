## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      v <- NULL
      set <- function(l) { ## this will be utilized if you want to reset the matrix
        x <<- l            ## apply a new matrix to x
        v <<- NULL         ## resetting v to NULL
      } 
      ##below will be getting the value of the matrix
      get <- function() x
      ##below will setting the inverse of the matrix
      setinversematrix <- function(InvMatrix) v <<- InvMatrix
      getinversematrix <- function() v
      ##now will be getting the inverse of the matrix 
      list(set = set, get = get,
           setinversematrix = setinversematrix,
           getinversematrix = getinversematrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## get the inverse of the matrix with the below code       
  v <- x$getinversematrix()
  
  ## check if there is the matrix in there   
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  ## if it is not: then get the inverse of the matrix   
  data <- x$get()
  v <- solve(data, ...)
  ## now you set the inverse of the matrix 
  x$setinversematrix(v)
  v
}
