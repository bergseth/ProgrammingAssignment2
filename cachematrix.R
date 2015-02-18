## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse



makeCacheMatrix <- function(x = matrix()) {

  m <- NULL                             ## Init m (inverse variable)
  
  set <- function(y) {                  ## 1 - Set the matrix
    x <<- y 
    m <<- NULL 
  } 
  
  get    <- function() x                ## 2 - Get the matrix
  setinv <- function(inv) m <<- inv     ## 3 - set the inverse
  getinv <- function() m                ## 4 - get the inverse

  list(set    = set,                    ## List of methods
       get    = get, 
       setinv = setinv, 
       getinv = getinv) 
}


## Write a short comment describing this function

## Computes the inverse of the matrix returned by "makeCacheMatrix"
## If inverse already calculated then print "getting cached data" and retrun cached version
## If not calculated, invert matix and return

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()                       ## Try to get inverted matrix
  if(!is.null(m)) {                     ## Test if inverted matrix exist
    message("getting cached data.") 
    return(m)                           ## Break & return inverted matrix 
  } 
  data <- x$get()                       ## Get the matrix
  m    <- solve(data)                   ## Invert the matrix
  
  x$setinv(m)                           ## Store the inverted matrix
  m                                     ## Return the inverted matrix
}


#### TEST
##> b = matrix(1:4, nrow=2, ncol=2)
##> x = makeCacheMatrix(b)
##
## Run #1 - setting chached data
##> cacheSolve(x)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
## Run #2 - getting chached data
##> cacheSolve(x)
##getting cached data.
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##

