## Put comments here that give an overall description of what your
## functions do

## First create the function to invert or retrieve

makeCacheMatrix <- function(x = matrix()) {
    INV <- NULL #not sure why I need this but it seems I do
    set <- function(y) {
        x <<- y
        INV <<- NULL #overwrites any value of INV in the cache
    }
    get <- function() x #not defined here so will come from parent enviro
    setinverse <- function(inverse) INV <<- inverse   #inputs value of inverse to INV in parent
    getinverse <- function() INV   #retrieves from parent enviro
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## Then invert the matrix, retrieving from cache if available

cacheSolve <- function(x, ...) {
    INV <- x$getinverse()
    if(!is.null(INV)) {
        message("getting cached data")
        return(INV)
    }
    matrix <- x$get()
    INV <- solve(matrix, ...)
    x$setinverse(INV)
    INV
    
}

## Testing the code to see if it works

m <- matrix(c(6,4,8,9,1,2,10,3,8), nrow=3, ncol=3)
cm <- makeCacheMatrix(m)
a <- cacheSolve(cm)
a
#then multiply to check for identity
round(a %*% m, 1)


