setwd("C:/Users/Vibhav/coursera/ProgrammingAssignment2")

## makeCacheMatrix returns a list with four functions - 
## 1: set - sets the value of the matrix
## 2: get - simply returns the orginal matrix
## 3: setinverse - stores (caches) the value of matrix inverse
## 4: getinverse - retrieves the chached inverse, if present, else returns NULL

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() {
                x
        }
        
        setinverse <- function(inverse) {
                i <<- inverse
        }
        
        getinverse <- function() {
                i
        }
        
        list(set = set, get = get,
             setmean = setinverse,
             getmean = getinverse)
}


## cacheSolve is a function where you pass the makeCacheMatrix function
## This returns the inverse of the matrix that was passed tot he makeCacheMatrix function
## NOTE: this function should be called once the makeCacheMatrix function has been stored in a variable
## It first checks whether the inverse is already cached. If it is not, the inverse is calculated and cached
## if found, the cached value is retrieved

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()                     # getting the cached inverse
        
        if(!is.null(i)) {                       # checking is the inverse was cached
                message("getting cached data")
                return(i)                       # returns cached inverse, if found
        }
        
        data <- x$get()
        
        i <- solve(data, ...)                   # calulates inverse, if not found
        
        x$setinverse(i)                         # stores the inverse in the cache for future use
        
        i
}
