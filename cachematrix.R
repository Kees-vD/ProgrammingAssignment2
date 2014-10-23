## First use makeCacheMatrix to store the matrix. Then the pass the returned object
## to cacheSolve as a parameter. Use cacheSolve to calculate the inverse of the matrix.

## Sets up function closure/environment with matrix data (parameter x), 
## its inverse (variable inverse) and the setter- and getter-functions
## for accessing the data in the environment.
makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      
      ## set and get functions for matrix
      set <- function (newmatrix) {
            x <<- newmatrix
            inverse <<- NULL
      }
      
      get <- function() x
      
      ## set and get functions for inverse calculation
      setinverse <- function (inverseP) inverse <<- inverseP      
      
      getinverse <- function () inverse
      

      ## return object with getter- and setter-functions
      ## (returned object also includes reference to environment)
      list(set = set, get = get, setinverse = setinverse
           , getinverse = getinverse )
      
}


## This function takes the returned list-object from makeCacheMatrix
## as a parameter. And calculates the inverse or returns the cached 
## inverse of the matrix that is in the function environment
## referenced in object x.
cacheSolve <- function(x) {      
      
      ## get inverse 
      solv <- x$getinverse()
      
      if(!is.null(solv)) {
            message("getting cached data")
            return(solv)
      }
      
      matr <- x$get()
      solv <- solve(matr)
      x$setinverse(solv)
      solv      
}
