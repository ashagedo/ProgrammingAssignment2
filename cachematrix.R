##To test these functions
##x<-matrix(c(31,24,38,21,32,44,30,40,50),3,3)
##funcs<-makeCacheMatrix(x)
##cacheSolve(funcs)
##cacheSolve(funcs)



##makeCacheMatrix
## A function that creates a list of functions used to cache the inverse 
## of a matrix


makeCacheMatrix <- function(x = matrix()) {
  
  ##'m', a placeholder for the inverse is set as NULL
  
   m <- NULL
  
  ##The function 'getx' retrieves the matrix x
  
   getx <- function() {
    x
   }
  
   ## The function setinverse makes 'm", the inverse of matrix x, 
   ##available globally
   
   setinverse <- function(inv) {
    m <<- inv 
   }
   
  ##The function 'getinverse' retreives the variable 'm'
   
  getinverse <- function() {
    m
  }
  ## The output of 'makeCacheMatrix' generates a list of 
  ##three functions; getx, setinverse and getinverse
  
  list(getx_var = getx, setinverse_var = setinverse, getinverse_var = getinverse)
}


## cacheSolve
##A function that computes the inverse of a matrix
##and stores it in cache

##cacheSolve brings in a list

cacheSolve <- function(function_list, ...) {
  
  ##The output of the 'getinverse' function is stored in the variable 'cache'
  
  cache <- function_list$getinverse_var() 
  
  ##If 'cache' is not NULL the function returns 'cache'
  
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  
  ##Otherwise the output of the 'getx' function is stored in the variable 'data'
  
  data <- function_list$getx_var()
  
  ##The variable 'cache' is assigned the inverse of the variable 'data'
  
  cache <- solve(data, ...)
  
  ##The variable 'cache' is fed into the function 'setinverse' through the 
  ##varaible set_inverse_var
  
  function_list$setinverse_var(cache)
  cache
}
