## overall description the functions :
## ----------------------------------
## The function makeCacheMatrix create a cache for a matrix with the argument name 
## x. It return a list of functions that allow to manupilate this matrix
## The function cacheSolve calculate the inverse of matrix wich was cached by the
## function makeCacheMatrix. The argument of cacheSolve is a list returned by 
## makeCacheMatrix
## 
## comment describing makeCacheMatrix function :
## The argument "x" is a matrix with a default value being empty matrix
## if the fuction is called without argument, it create a matrix with NULL value
## The function return a list of four function. They use the operator <<- to modify
## the value of variables that are in the scope of parent environnement up to global
## environnement, it return a list of function :
## The function set the value of the matrix
## The function setInverse set the value of the inverse matrix
## The get function return a matrix
## The getInverse return the caching inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        setInverse <- function(Inverse) m <<- Inverse
        
        getInverse <- function() m
        
        list(set = set, 
             get = get,
             setInverse = setInverse ,
             getInverse = getInverse )
        
}

##
## short comment describing this function cacheSolve :
##This argument named x is the list returned by makeCacheMatrix There is no 
## default value. It return the inverse of the matrix
## First it call the function getInverse. If the value returned is not null
## that's mean that's the inverse of the matrix and no need to calculate it.
## it write a message ande return this value. Otherwise it calulate the inverse,
## cached it and return the inverse of the matrix
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
        
}

