# Caching matrix inversion
# Maksym Gontar
# max.gontar@gmail.com
# June 2015

## Create special object that contain matrix and its inverse cache
makeCacheMatrix <- function(x = matrix()) {

    ## Initialize variable for matrix inverse value
    i <- NULL
    
    ## Function to set matrix value
    set <- function(y) {
        
        ## If assigned matrix value is same as current value, 
        ## skip assignment and cache clear
        if(!identical(x, y))
        {
            
            ## Assign matrix value
            x <<- y
            
            ## Clear inverse cache value
            i <<- NULL
        }
    }
    
    ## Function to get matrix value
    get <- function() x
    
    ## Function to set matrix inverse cache value
    setInv <- function(inv) i <<- inv
    
    ## Function to get matrix inverse cache value
    getInv <- function() i
    
    ## Return special object that contain matrix, its inverse cache
    ## and functions for setting and getting these values
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## Compute inverse of matrix from special object
cacheSolve <- function(x, ...) {
    
    ## Get matrix inverse cache
    i <- x$getInv()
    
    ## If there is no cached value for inverse, it is calculated
    if(is.null(i)) {
        
        ## Get matrix value
        data <- x$get()
        
        ## Calculate matrix inverse using solve() function
        i <- solve(data, ...)
        
        ## Set calculated matrix inverse to the cache
        x$setInv(i)
    }
    
    ## Return matrix inverse value
    i
}