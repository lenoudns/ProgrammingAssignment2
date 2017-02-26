## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
			x <<- y
			i <<- NULL
	}
	get <- function() x
	setinverseMatrix <- function(inverseMatrix) i <<- inverseMatrix
	getinverseMatrix <- function() i
	list(set = set, get = get,
		 setinverseMatrix = setinverseMatrix,
		 getinverseMatrix = getinverseMatrix)
}

## computes the inverse of the special "matrix", return from cache if already computed
cacheSolve <- function(x, ...) {
	i <- x$getinverseMatrix()
	if(!is.null(i)) {
			message("getting cached data")
			return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setinverseMatrix(i)
	i
}

#TEST THE FUNCTIONS
x = rbind(c(2, 3), c(4, 5))
cachematrix = makeCacheMatrix(x)
cachematrix$get()
#     [,1] [,2]
#[1,]    2    3
#[2,]    4    5
#no cache yet
cacheSolve(cachematrix)
#     [,1] [,2]
#[1,] -2.5  1.5
#[2,]  2.0 -1.0
#get from cache
cacheSolve(cachematrix)
#getting cached data
#     [,1] [,2]
#[1,] -2.5  1.5
#[2,]  2.0 -1.0




