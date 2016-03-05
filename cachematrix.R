##This method takes the matrix as input and returns special list, 
##with length 4, in which each object is specific function.
 
makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinverseMatrix <- function(inverseMatrix) im <<- inverseMatrix
        getinverseMatrix <- function() im
        list(set = set, get = get,
             setinverseMatrix = setinverseMatrix, 
             getinverseMatrix = getinverseMatrix) 
}

##This method solve the inverse of matrix given as input to the makeCacheMatrix.
##Input for this function is the special list created (returned) from 
##makeCacheMatrix function. Inverse of matrix is stored in cached, so that
##avoiding recomputation.
   
cacheSolve <- function(x, ...) {
        im <- x$getinverseMatrix()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinverseMatrix(im)
        im
}