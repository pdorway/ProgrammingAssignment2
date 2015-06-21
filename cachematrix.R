##################################################################
## Description: The functions will allow the user to store the inverse 
## of a matrix in another environment so it can be globaly visible 
## to other functions without recalulation.
#
## makeCacheMatrix - Takes a matrix and returns a set of functions you can use to store the inverse in a cache.
## setmatrix - set the matrix
## getmatrix - display the current matrix
## setinverse - inverse the current matrix
## setinverse - display the inverse in cache
##
## cacheSolve - Checks the cache for the inverse and returns either the cached value or calculated value.
##
##
##
##Usage Example:
## size  <- 5
## my_matrix <- matrix(rnorm(size*size), nrow=size)
##
## my_matrix_cached <- makeCacheMatrix(my_matrix)
## my_matrix_cached$setmatrix(my_matrix)
## my_matrix_cached$getmatrix()
##
####Calculate the inverse
## Sys.time()
## my_matrix_inversed <- cacheSolve(my_matrix_cached)
## Sys.time()
##
####Calculate the inverse again - Value should return faster
## Sys.time()
## my_matrix_inversed <- cacheSolve(my_matrix_cached)
##S ys.time()
##
##
##
######################################################

#This function takes a matrix and returns a set of functions you can use to store the inverse in a cache.
makeCacheMatrix <- function(the_matrix = matrix()) {
    
    # Set the local variable to hold the inverse
    inversed_matrix <- NULL
    
    #Function to intialize the global(cached) variables
    setmatrix <- function(y) {
        
        #Cache the matrix
        the_matrix <<- y
        
        #Set the variable to hold the inverse in cache
        inversed_matrix <<- NULL
        
    }
    
    #Function to display the current matrix
    getmatrix <- function() the_matrix
    
    #Function to get the matrix inverse and cache the value
    setinverse <- function(inverse) inversed_matrix <<- inverse
    
    #Functioin to display the inverse currently in cache
    getinverse <- function() inversed_matrix
    
    #Return the list of functions
    list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse) 
    
}

#This function checks the cache for the inverse and returns either the value in cache or calculated value.
cacheSolve <- function(the_matrix, ...) {
    
    #Retrieve the inverese currently in cahce
    inversed_matrix <- the_matrix$getinverse()
    
    #If the inverse cache has a value(not null) then return the value  
    if(!is.null(inversed_matrix)) {
        message("Message: Using the cached inverse value.")
        return(inversed_matrix)
    
    #Otherwise caculate the inverse,save it to the cache and return the value
    } else {
    
        #Get the new matrix
        new_matrix <- the_matrix$getmatrix()
        
        #Calculate the inverse
        inversed_matrix <- solve(new_matrix, ...)
    
        #Store the value in cache
        the_matrix$setinverse(inversed_matrix)
        
        #Return the inverse
        inversed_matrix
        
    }
    
}



