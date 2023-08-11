## The functions below create a way to store and retrieve inverted matrices
## The first one assigns functions to an object using a list
## The second one actually solves a matrix, unless it is already available 


# The function below creates an object with functions that can store and
# retrieve matrices

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    set_inv <- function(inv) i <<- inv
    get_inv <- function() i
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


# The function below checks whether the inverse of the matrix is already cached.
# If so, it returns that object. If not the inverse is calculated and stored. 

cacheSolve <- function(x, ...) {
    i <- x$get_inv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$set_inv(i)
    i
}




## TESTING 

m <- matrix(1:4, 2, 2)
m_s <- solve(m)

in_mat <- makeCacheMatrix(m)
cacheSolve(in_mat)      # inverse is stored

cacheSolve(in_mat)      # second time, matrix is retrieved from cache






