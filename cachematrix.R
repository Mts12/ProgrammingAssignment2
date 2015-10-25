## [Overview]
## This source provides method to avoid computing 
## the same inverse matrix which was computed before.
##
## [Restriction]
## This assignment assumes that given matrix is always invertible.
##


## This function manages matrix and inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    
    ## Register a matrix which should be inversed.
    setMatrix <- function(y){
        x <<- y
        x_inv <<- NULL
    }
    
    ## Return registerd matrix.
    getMatrix <- function(){
        x
    }
    
    ## Cache a inverse matrix.
    setInvMatrix <- function(y_inv){
        x_inv <<- y_inv
    }
    
    ## Return cashed inverse matrix.
    getInvMatrix <- function(){
        x_inv
    }
    
    ## Check if an inversed matrix was cached.
    isInverted <- function(){
        if(is.null(x_inv)){ 
            return (FALSE)
        }else{
            return (TRUE)
        }
    }
    
    list(setMatrix    = setMatrix, 
         getMatrix    = getMatrix, 
         setInvMatrix = setInvMatrix, 
         getInvMatrix = getInvMatrix,
         isInverted   = isInverted
    )  
}


## This function computes inverse of the matrix 
## when it isn't computed yet.
cacheSolve <- function(x, ...) {
    ## check if the inverse matrix is cached.
    if(!x$isInverted()){
        x$setInvMatrix( solve( x$getMatrix(), ... ) )
    }
    
    x$getInvMatrix()
}
