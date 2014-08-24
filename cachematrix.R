## Programming Assignment 2


# These functions cache the inverse of a matrix

# Part 1: makeCacheMatrix creates a "special" matrix
makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <- function(matrix) {
        m <<- matrix
        i <<- NULL
    }
    get <- function() {
        m
    }
    setInv <- function(inverse) {
        i <<- inverse
    }
    getInv <- function() {
        i
    }
    list(set=set, get=get
         setInv = setInv
         getInv = getInv)
}


# Part 2: cacheSolve creates the inverse of the "special" matrix
cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    dat <- x$get()
    m <- solve(dat) %*% dat
    x$setInv(m)
    m
}

