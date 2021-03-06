## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	## Initialize values
        m <- NULL

	## Push the matrix into variable x and
	## initialize the m to null as the inverse is not yet calculated
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

	## Get the matrix from variable x
        get <- function() x

	## Set the inverse of the matrix and put it in cache
        setinv <- function() m <<- solve(x)

	## Get the inverse of the matrix from cache (if available)
        getinv <- function() m

	## Return the various results as a list
        list	( set = set
		, get = get
		, setinv = setinv
		, getinv = getinv
		)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	
	st_time<-Sys.time()

	if (is.null(x$getinv())) {
		print ("Creating a new inverse")
		x$setinv()
	}
	else {
		print("Reading the inverse from cache")
	}
	
	print (Sys.time() - st_time)
	return(x$getinv())

}

## Define the number of columns and rows
nr_row_cols <- 1000

## Create a square matrix with random values
r <- rnorm(nr_row_cols^2)
mymatrix <- makeCacheMatrix(matrix(r, nr_row_cols, nr_row_cols))

## Get the inverse (Create it)
inversematrix_create<-cacheSolve(mymatrix)

## Get the inverse (Get it from cache)
inversematrix_cache<-cacheSolve(mymatrix)

## Check the results; they should be equal
str(inversematrix_create)
str(inversematrix_cache)


