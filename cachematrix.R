# The firstfunction(makecacheMatrix) creates the list;which is responsible for 
# giving access to its methods(subfunctions) from OUTSIDE of the coded.
# The second # function(setInverse) checks if the inverse has already been 
# cached or not. If not then it calcuates the inverse and caches it.

# This function creates a list to access all the functions of makeCacheMatrix 
# function so that all the subfunctions can be accessed from outside.The list 
# gives access to four functions:
#1.set the value of the vector.
#2.get the value of the vector.
#3.set the value of the inverse.
#4.get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL   ##Initialize to NULL
        set <- function(y) ##to change the value of x.
        {    
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() {x}
        
        setInverse <- function(inverse)## to set the inverse
        {
                inverse <<- inverse  
        }
        
        getInverse<- function() ##to get the inverse.
        {
                inverse
        } 
        
        list(get = get,
             set = set,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function checks if the inverse of the given matrix is cached or not. If
## not then it calculates and caches it.It returns the inverse.

cacheSolve <- function(x, ...) {
        
        inverse <- x$getInverse()
        
        if(!is.null(inverse))   ##if cached value present then getting it.
        {
                message("getting cached Inverse")
                return(inverse)
        }
        data <- x$get()     ##get the value of x.
        inverse <- solve(data) ##solve returns the inverse of x.
        x$setInverse(inverse) ##setting the inverse.
        
        inverse     ## Return a matrix that is the inverse of 'x'
        
}
