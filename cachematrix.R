## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
library(matlib)
makeCacheMatrix <- function(x = matrix()) {
       if(ncol(x) == nrow(x) && det(x) != 0){
               inverse_matrix <- NULL
               set <- function(y){
                       x <<- y
                       inverse_matrix <<- NULL
               }
               get <- function() x
               set_inverse <- function() inverse_matrix <<- inv(x)
               get_inverse <- function() inverse_matrix
               list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
       } else {
               return(message("The matrix is'n invertible."))
       }
       

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_matrix <- x$get_inverse()
        if(! is.null(inverse_matrix)){
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- inv(data, ...)
        x$set_inverse()
        inverse_matrix
        
        
}














