#creating a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #setting value of matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #getting value of matrix
    get <-function() x
    #setting value of inverse
    setinverse <- function(solve) m <<- solve
    #getting value of inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#function computing the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse already calculated then it will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    #getting the inverse
    m <- x$getinverse()
    #checking if inverese is calcualted and if yes returning it, if not computing it.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}


