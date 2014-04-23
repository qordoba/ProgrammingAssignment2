## first function will mainly set the inverse to Null
## and define the rest of the functions 

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
              x <<- y
              inv <<- NULL
            }
            get <- function() x
            setinv <- function(inverse) inv <<- inverse
            getmean <- function() inv
            list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Here the function checks if the inverse is cahched already or not, and if it's cached
## then it's retrieved automatically without re-calculating it again. 
## If it's not calculated, then it calculates the inverse for it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getmean()
          if(!is.null(inv)) {
                  message("getting cached data")
                  return(inv)
          }
          data <- x$get()
          inv <- solve(data, ...)
          x$setinv(inv)
          inv
}
