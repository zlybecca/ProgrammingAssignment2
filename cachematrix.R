## THis function createds a special "matrix" object that can cache its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ##Initialize the inverse vector
        inv <-NULL
        ##Set the matrix
        set <- function (y){
                x <<-y
                inv <<-NULL
        }
        ## Get the matrix
        get <- function() x
        ##Set the inverse of the matrix
        setInverse <- function(f) inv <<- f
        ##Get the inverse of the matrix
        getInverse <- function() inv
        list (set=set, get = get,
              setInverse = setInverse,
              getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##If the inverse has already been calculated, then the cachesolve should retrive the inverse from the cache

cacheSolve <- function(x, ...) {
        ##Return the inv which is the reverse of 'x'
        inv <- x$getInverse()
        ##Return inv if its already calculated
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        ##Get the matrix
        data <- x$get()
        ##Calculate the inverse of a matrix
        inv <- solve(data, ...)
        ##Set the inverse to inv
        x$setInverse(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}
