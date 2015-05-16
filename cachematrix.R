## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())
{
    inv <- NULL

    return(
        list(
            get = function() {
                return(x)
            },
            set = function(new_x) {
                x   <<- new_x
                inv <<- NULL
            },
            getInverse = function() {
                return(inv)
            },
            setInverse = function(new_inv) {
                inv <<- new_inv
            }
        )
    )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...)
{
    inv <- x$getInverse()

    if (is.null(inv))
    {
        inv <- solve(x$get())
        x$setInverse(inv)
    }

    return(inv)
}
