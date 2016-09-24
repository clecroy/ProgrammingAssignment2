## These functions cache the inverse of a matrix to avoid excessive computation. If a matrix inverse has laready been calculated, the inverse will be cached so that we do not have to calculate it again. 

##  makeCacheMatrix creates a matrix-like object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      #set inv holder to null
      inv <- NULL
      #function to retrieve data
      getx <- function() x
      #function to set calculated inverse as inv globally
      setinverse <- function(inverse) inv <<- inverse
      #function to retrieve inv
      getinverse <- function() inv
      #return list of functions
      list(getx = getx, setinverse = setinverse,
           getinverse = getinverse)

}


## cacheSolve calculates the inverse of the matrix-like objects created by makeCacheMatrix unless the inverse has already been calculated, in which case it looks for the inverse in the cache.

cacheSolve <- function(x, ...) {
      #attempt to retrieve inv
      inv <- x$getinverse()
      #if inv exists, return it (cached inverse)
      if(!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
      }
      #if inv doesn't exist, set input data to data object
      data <- x$getx()
      #calculate inverse and set to inv
      inv <- solve(data)
      #set inverse as inv globally
      x$setinverse(inv)
      #return inv
      inv
}
