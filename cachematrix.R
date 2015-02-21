## THE PURPOSE OF THIS CLASS IS TO STORE AND RETURN A CACHED VERSION OF A MATRIX
## THIS MATRIX IS AN INVERTED VERSION OF THE INCOMING PARAMETER

## THIS FUNCTION CREATES THE PUBLIC PROPERTIES AS A LIST. THE LIST CONTAINS THE "GET" AND "SET" METHODS TO BE USED AGAINST THE MATRIX
makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    set <- function(y) 
    {
        x <<- y
        m <<- NULL
    }
    get <- function() 
    {
        x
    }
    
    setmean <- function(mean) 
        m <<- mean
    getmean <- function() 
        m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}


makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmean(m)
    m
}