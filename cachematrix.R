#The overall goal of this functions is Caching the Inverse of a Matrix

#This function creates a special "matrix" object that can cache its inverse

#First we set the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set<- function(z) {
      x <<- z
      inv <<- NULL
    }
#The second step is to get the value of the matrix
    get <- function () x;
    setinverse <- function(inverse) inv <<- inverse #The third step is to set the value of the inverse 
    
# The fours step is to get the value of the inverse
    getinverse <- function () inv;
      list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


#The functions computes the inverse of the special "matrix" returned by makeChaceMatrix
#if the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.

#checking to see if the inverse has been calculated if so then should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {  
  ## Return a matrix that is the inverse of 'x'
   inv<-x$getinverse()
   if(! is.null(inv)) {
            message("retrive cached matrix data")
            return(inv)
   }
   
#Calculates the inverse of data and sets the value of the inverse
   data <- x$get() 
    inv<- inverse(data, ...)
    x$setinverse(inv)
    inv
   
      
}
