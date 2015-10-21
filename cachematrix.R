## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function assigns names to the matrix, solve function 
## result and cached values for each matrix.  The set portion of the function 
## allows the user to change the matrix within the function.   

## The cacheSolve function searches for a cached value of the solve function 
## for a provided matrix. If a cached value is present the function returns it,
## if not, the solve function is used to the inverse of a given matrix. 


## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL  ## variable m assigned NULL in the local environment 
  set <- function(y) {   ## fxn allows you to change the matrix 
    x <<- y    ## variable x (matrix) assigned in the containing environment 
               ## (within the set function)
    m <<- NULL ##  assign m to be NULL so cacheSolve will recognize it as a new 
  }
  get <- function() x ## assigns get to x (your matrix)
  setinver <- function(solve) m <<- solve  ## assigns setinver to the function 
                                           ## solve and caches computed values as m
  getinver <- function() ## assigns getiver to the cached value 
  list(set = set, get = get,  ## list of arguments to pass to the ... in cacheSolve 
       setinver = setinver,
       getinver = getinver)
}

## Write a short comment describing this function  

cacheSolve <- function(x, ...) {  ## look at x, and the listed values 
  m <- x$getinver()         ## assign m to getinver for x
  if(!is.null(m)) {        ##determine if the inverse of the matrix has already 
                           ## been computed
    message("getting cached data") # if true give this message 
    return(m)     # give the inverse 
  }
  data <- x$get()  ## if the matrix has no cached inverse, get the matrix
  m <- solve(data, ...) ## assign m to the result of the solve function in local e
  x$setinver(m)   ## compute the the inverse 
  m     ## print the inverse
}  





