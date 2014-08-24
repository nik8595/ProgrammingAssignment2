#Matrix inversion is usually a costly computation. We can
#cache the solutions to help save CPU time. If we do not have a
#cached solution, we must solve it then cache it.
#These functions manage that task.

#This function will prepare a matrix for cacheSolve.
#It adds the properties "get" and "set".
makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL # holds inverse martix
  
  set <- function(y) # sets the matrix value
  {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  get <- function() { x } # gets the matrix value
  
  setInverse <- function(inverse) { inverseMatrix <<- inverse } #sets the inverse matrix value
  
  getInverse <- function() { inverseMatrix }#gets the inverse matrix value
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  #explains the function if requested
}

#This function checks the cache for a previously calculated
#solution. If no solution was found in the cache, then create it.
cacheSolve <- function(x, ...) {
  m <- x$getInverse() #get the inverse of this matrix from cache
  if(!is.null(m)) #if we found it in cache, we're done
  {
    message("getting cached data")
    return(m)#exits function and return the inverse matrix 'm'
  }

  #we need to find the inverse
  myMatrix <- x$get()
  m <- solve( myMatrix )#this finds the inverse
  x$setInverse(m)#cache the inverse found
  m  #exits function and return the inverse matrix 'm'
}