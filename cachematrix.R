##The following is to create a function that stores the value of a matrix and caches its inverse

##The first function is to store a matrix and caches its inverse, 
##which is really a list containing a function to set and get the value of a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##The second function is to calculate the inverse of the matrix returned above.
##If the inverse already exists, then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
  i <- x$getinverse()
  if (!is.null(i)) { ##check if the inverse already exists
    message("getting cached data")
    return(i) ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  i <- solve(data, ...) ##calculate the inverse of matrix 'x'
  x$setinverse(i)
  i
  
}
