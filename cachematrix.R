## The following program will cache the inverse of a given matrix and use it 
## next time if available. Otherwise it will calculate and cache it.

## makeCacheMatrix returns a list of functions, which could be called in 
## cacheSolve function either to give the inverse or store the calculated one.

makeCacheMatrix <- function(x = matrix()) {
  i <-matrix(nrow =nrow(x), ncol = nrow(x))
  set <- function(y) {
    x <<- y
    i <<-matrix(nrow =nrow(x), ncol = nrow(x))
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Applying the functions of makeCacheMatrix, 
## the following function will get or calculate and cache the inverse of the matrix. 
## It is important to note that the argument is not the matrix itself,
## but a variable, e.g. whatever <- makeCacheMatrix(our matrix).
## cacheSolve(whatever) will give us the required result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(all(!is.na(i))) {        # since not a single value should be examined, 
                              #I used the function all() to check 
                              # if all of the values are NAs
    message("getting cached data")
    return(i)
  }
  matrixdata <- x$get()
  i <- solve (matrixdata)
  x$setinverse(i)
  i
}

