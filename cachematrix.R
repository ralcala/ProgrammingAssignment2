#' Create a matrix cache object.
#'
#' The cache matrix object contains the matrix passed by parameter but it also can cache its inverse.
#' A list of methods for interfacing is returned and used by the \code{\link{cacheSolve}} matrix.
#'
#' @param  x A numeric matrix.
#' @return A list of methods for interfacing with the cache matrix object.
#'
#' @examples
#' mat <- makeCacheMatrix(matrix(rnorm(1000000), 1000, 1000))
#'
#' @seealso \code{\link{cacheSolve}}
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

#' Invert a matrix stored in a cache matrix object.
#'
#' The inverse matrix stored inside the cache matrix object (build with
#' \code{\link{makeCacheMatrix}}) is extracted. If it's null
#' (not cached) it's calculated and stored for later quick access.
#'
#' @param  x   A cache matrix object.
#' @param  ... An optional list of parameters (currently not used).
#' @return The inverse of the matrix.
#'
#' @examples
#' mat <- makeCacheMatrix(matrix(rnorm(1000000), 1000, 1000))
#'
#' @seealso \code{\link{makeCacheMatrix}}
cacheSolve <- function(x, ...)
{
    inv <- x$getInverse()

    # If the inverse matrix is not available calculate it
    # and store in the cache matrix object.
    if (is.null(inv))
    {
        inv <- solve(x$get())
        x$setInverse(inv)
    }

    return(inv)
}
