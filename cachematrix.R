makeCacheMatrix <- function(x=matrix()){
      m<-NULL
      set<- function (y) {
            x <<- y
            m <<- NULL  ## restores inverse matrix value to null, as new inverse is recalculated.
      }  ##Changes the matrix sored in main function.--only used if matrix is changed.
      
      get <- function () x  ##returns matrix x stored in main function.
      setinverse <- function(solve) m <<- solve  ##calculates inverse of matrix(x) and stores it
      getinverse <- function() m    ##gets inverse of matrix(x)
      list(set = set, get = get, 
           setinverse = setinverse,
           getinverse = getinverse)
}


cacheSolve <- function(x, ...){   #Stores makeCacheMatrix.
      m <- x$getinverse()
      if(!is.null(m)){
            message("getting cached data")
            return(m)    ##returns inverse of matrix if not null and in cache.
      }
      data <- x$get()
      m <- solve(data, ...)  ## solves inverse of new matrix
      x$setinverse(m)  ## stores inverse of new matrix in cache.
      m
}