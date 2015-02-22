## These functions together create a special matrix object that can cache its inverse, and then check and see if 
## a particular matrix has been stored in cache. If it is, cacheSolve retrieves it. If not, it calculates it and 
##stores it into setinverse() in makeCacheMatrix()

## makeCacheMatrix creates a special matrix object that can cache its inverse. The output is a list of functions, set,
##get, setinverse, and getinverse. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ##once we are in makeCacheMatrix, we start off by setting m to NULL.
    
    set <- function(y){
      ##This function sets the value of the cached martrix. It takes the value of Y, assigns it to X, but leaves m Null. 
      x<<- y
       m<<- NULL
  }
  
    get <-function() x   ##gets the value of x, which we set using the set function. 
    setinverse <-function(inverse) m <<- inverse ##takes the inverse of the matrix, gained from cacheSolve, and assigns it to m.
    getinverse <- function() m
    list(set = set, get=get, setinverse = setinverse, 
         getinverse = getinverse)
        ##creates that special list of functions we will call on in cacheSolve.
}


## cacheSolve checks to see if the inverse of m is already stored in makeCacheMatrix's m. if it is, it retrieves it.
## If not, it finds the inverse and sets it into makeCacheMatrix using setinverse()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cache_inverse<- x$getinverse()
    if(!is.null(cache_inverse)){
        message("getting cached data")
        return(cache_inverse)
    }
    
    ##the if statement above checks to see if the inverse is  already stored in makeCacheMatrix. if it is, it retrieves it.
    data <- x$get() ##retrieves the matrix from makeCacheMatrix
    m <- solve(data, ...)
    ##finds the inverse of the matrix
    x$setinverse(m)
    ##sets the inverse into makeCacheMatrix using setinverse()
    m
    ##returns the inverse. 
}
