## The following function makeCacheMatrix creates a list containing:
## 1. sets empty matix
## 2. gets value of matix
## 3. sets value of matix-inverse
## 4. gets value of matix-inverse


makeCacheMatrix <- function(x = matrix()) { 
    ## initialize matrix inverse to NULL
    mat_inverse <- NULL                     
    ## make another function where the value will be cached in 1. Matrix is created
    ## for the first time. 2. changes made to cached matrix
    set <- function(y) {                      
        x <<- y
        ## change the value of inverse of the matrix in case the matrix was changed.
        mat_inverse <<- NULL              
    }
    ## gets the value of the inverse
    get <- function() x                           
    #calculates the inverse 
    setinverse <- function(solve) mat_inverse <<- solve 
    # gets the inverse     
    getinverse <- function() mat_inverse        
    ## passes the value of the function makeCacheMatrix        
    list(set = set, get = get,                    
         setinverse = setinverse,
         getinverse = getinverse)
}

# used to get the cache of the matrix with above function
# gets it from cache if alread calculated
cacheSolve<- function(x, ...) {                 
    mat_inverse <- x$getinverse()
    #if the inverse exists, it gets it.
    if(!is.null(mat_inverse)) {                 
        message("getting cached data - Inverse of the matrix")
        return(mat_inverse)
    }
    #if the inverse if not there, first it is calculated and then retrieved.
    data <- x$get()                               
    mat_inverse <- solve(data, ...)
    x$setinverse(mat_inverse)
    mat_inverse
}
