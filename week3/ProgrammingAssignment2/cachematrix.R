# cache the matrix
makeCacheMatrix <- function(theMatrix = matrix()) {
        invertedMatrix <- NULL
        
        # basic functions
        get <- function() { return(theMatrix) }
        set <- function (y) {
                matrix <<- y
                invertedMatrix <<- NULL
        }
        
        # inverting all
        getInvertedMatrix <- function() { return( invertedMatrix ) }
        setInvertedMatrix <- function(aInvertedMatrix) { invertedMatrix <<- aInvertedMatrix }
        
        # create the list with the returns
        list(get = get, set = set, setinverse = setInvertedMatrix, getinverse = getInvertedMatrix)
}

# resolve the cache for the inverted

cacheSolve <- function(aCachedMatrix, ...) {
        
        cachedInverted <- aCachedMatrix$getinverse()
        
        if (!is.null(x = cachedInverted)) {
                print("cache hit! (:")
                return(cachedInverted)
        }
        
        print("cache miss! ):")
        
        tryCatch({
                # try to set the cached version
                aCachedMatrix$setinverse( solve( aCachedMatrix$get(), ... ) )
                },
                error = function(e) {
                        message("Error! Problably it's impossible to invert the matrix.")
                        message(e)
                        
                        return(NA)
                }
        )
        
        ## Return a matrix that is the inverse of 'x'
        return( aCachedMatrix$getinverse() )
}
