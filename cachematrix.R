## Put comments here that give an overall description of what your
## functions do
#  The pair of functions creates a special object which stores a matrix and
#  may cache its inverted matrix. If inverted matrix is already cached, no 
#  calculation is needed, otherwise it will calculate and cache the inverted 
#  matrix.

## Write a short comment describing this function
#  makeCacheMatrix creates a special "matrix", 
#  which is really a list containing a function to
#  1. set the value of the matrix
#  2. get the value of the matrix
#  3. set the value of the inverted matrix
#  4. get the value of the inverted matrix
makeCacheMatrix <- function(x = matrix())
{
    m <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }   
    get <- function() x

    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m

    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}



## Write a short comment describing this function
#  The following function calculate the inverted matrix of the given matrix
#  However, it first checks if the inverted matrix is already calculated. If
#  so it will directly fetch the matrix, otherwise it will calculate inveted
#  matrix and cache the matrix with setmatrix() function
cacheSolve <- function(x=matrix(), ...)
{
    m <- x$getmatrix()
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}
