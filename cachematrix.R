
# With this function we create object
# which contains fuctions which allow us
# to manage our matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL                         # resetting inversion
      set <- function(y) {                # set matrix
            x <<- y 
            inv <<- NULL
      }
      get <- function() x                 # returning matrix
      setInverse <- function(inverse) {   # setting inverse
            inv <<- inverse
      }
      getInverse <- function() {          # getting inverse
            inv
      }
      list(set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}

# With this function we can get inverse of
# our matrix. If it was calculated before,
# we just return it, else we write an inverse 
# in particular variable and only after that 
# return it.
cacheSolve <- function(x, ...) {
      inv <- x$getInverse()      # try to get inverse from object
      if(!is.null(inv)) {        # if it was calculated before, we return it
            print("Returning cached inverse...")
            return(inv)
      }
      inv <- solve(x$get(), ...) # else we calculate the inverse 
      x$setInverse(inv)          # and set it to variable in our object
      
      inv                        # the we return it
}
