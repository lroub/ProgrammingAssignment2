#####################################################################
#cachematrix.R 
#Coursera R Programming-Week 3 Assignment
#
#Author:lroub
#Submission Date: August 14, 2018
#####################################################################

## Pair of functions that cache the inverse of a matrix object
## to avoid having to compute the inverse every time. 
## Please note, all matrices are assumed to be square. 


## makeCacheMatrix-Creates a matrix object and caches its inverse 
makeCacheMatrix <- function(x = matrix()) {
  
    minv <- NULL     #set inverse as null
    set <- function(y) { 
      x <<- y       # assigns input argument y to object x in the parent environment
      minv <<- NULL #set inverse as null (to clear prior inverse values cached by cacheSolve, if any)
    }             
    get <- function() x #retrieves matrix object x from parent environment
    setinv <- function(solve) minv <<- solve #assigns input argument (solve) to minv. Solve calculates the inverse.
    getinv <- function() minv #retrieves inverse value of matrix  
    
    ##assigns each set/get function as an element in a list and returns it to parent environment
    list(set = set, get = get,   
         setinv = setinv,          
         getinv = getinv)
}


## cacheSolve-Computes the inverse of the matrix object returned by makeCacheMatrix.
## However, if inverse has already been calculated (and the matrix has not changed), 
## the inverse is retrieved from the cache and is not re-computed. 

cacheSolve <- function(x, ...) {
  
  ## get inverse of the matrix object x 
  minv <- x$getinv() 
  
  ## This chunk tests to see if there is a cached inverse and retrieves it, if available
  if(!is.null(minv)) { 
    message("retrieving matrix inverse from cache") 
    return(minv) 
  }
  
  ##If inverse is not available, the following lines will retrieve the matrix object x, calculate its inverse, and
  ## assign inverse to minv
  data <- x$get() 
  minv <- solve(data, ...) 
  x$setinv(minv) 
  minv    
}

