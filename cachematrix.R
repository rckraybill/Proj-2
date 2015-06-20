## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix can set a matrix, get the cached matrix, 
## cache a value for the inverse of the matrix, 
## and retrieve the value for the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set <- function(y){
                x<<-y
                m<<-NULL
        }
        
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        
        list(   set = set,
                get = get,
                setinv = setinv,
                getinv = getinv
        )
        
        
        
}


## This function will determine if an inverse of the given inverse
## matrix already exists, if so, it will return the cached matrix.
## If a cached inverse matrix does not exists, 
## it will calculate the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        
}
