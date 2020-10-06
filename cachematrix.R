## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #initializes as an object within the function environment to be used by later code (avoid errors)
  
  set <- function(y) { #sets/mutates values for the matrix and its inverse outside the function environment for caching later
    x <<- y
    i <<- NULL 
  }

  ##Lexical scoping: since the symbol x is not defined within get(), R retrieves it from the parent environment of makeVector().
  
  get <- function() x 
  setinverse <- function(solve) 
    i <<- solve #inverses the matrix in the function environment, which is then stored outside for cacheing as i
  
  getinverse <- function() i #creates a function that uses i (solve x)
  
  ##Naming the list elements is what allows us to use the $ form of the extract operator to 
  ##access the functions by name rather than using the [[ form of the extract operator, as in myVector[[2]]()
  
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
  i <- x$getinverse() #uses getinverse from above to look for values for i previously derived for x
  
  if(!is.null(i)) { #If the object i is not null (so it exists), then this returns that value of i
    message("getting cached data")
    return(i)
  }
  
  data <- x$getinverse() #If the object i is empty, so no i is found, then the solve function is executed
  i <- solve(data, ...) #it is then stored to i
  x$setinverse(i) #the value is set/added for future calculations
  
  i #prints the inverse of either method
  
}
