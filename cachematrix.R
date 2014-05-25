## Presentation

## Given that matrix inversion is usually a costly computation, I propose two functions that allow caching the inverse of a matrix instead of compute it
## repeatedly. The first one is called makeCacheMatrix, and the second is named cacheSolve.

## The "makeCacheMatrix" function creates in five steps a special "matrix" (which is actually a list) that contains a function to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ##First step: setting the value of a matrix

  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##Second step: getting the value of the matrix
  get <- function() x
  
  ##Third step: setting the value of the inverse of the matrix
  
  setinverse<- function(solve) m <<- solve
  
  ##Fourth step: getting the value of the inverse of the matrix
  getinverse <- function() m
  
  ##Fifth step: returning the "special" matrix (a list)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The "cachesolve" function returns in six steps the inverse of 'x', the "special" matrix (which is actually a list) created by the "makeCacheMatrix" function defined above. If the inverse has already been calculated (and the matrix has not changed), then the "cachesolve" function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## First:retrieve inverse matrix from cache 
  m <- x$getinverse()

## Second: if the inverse matrix was retrieved from cache, return it to caller
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }

## Third step: if there is not an inverse matrix to retrieve from cache, then get the value of the matrix
data <- x$get()

##Fourth step: solve the value of the inverse of the matrix 

m <- solve(data, ...)

##Fifth step: set the value of the inverse of the matrix

x$setinverse(m)

## Sixth step: return inverse matrix

m
}
