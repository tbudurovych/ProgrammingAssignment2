## Functions below allow computing of the matrix's inverse 
## and caching the result for subsequent calls

## Function takes a matrix as an input and returns a special
## cached matrix structure (list)
makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   
   ## setter of matrix
   set <- function(y){
      x <<- y
      i <<- NULL
   }
   
   ## getter of matrix
   get <- function() x
   
   ## setter of inverted
   setInv <- function(inverted) i <<- inverted
   
   ## getter of inverted
   getInv <- function() i
   
   ## returned structure
   list(set = set, get = get,
        setInv = setInv,
        getInv = getInv)
}


## Function computes an inverse of the matrix
## It takes cached matrix as an put
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   
   ## try get from cache
   i <- x$getInv()
   if (!is.null(i)){
      message("getting from cache")
      return (i)
   }
   ## else get and save in cache for a later use
   message("computing inverse")
   originalMatrix <- x$get()
   i <- solve(originalMatrix)
   x$setInv(i)
   i
}