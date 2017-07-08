##############################################################################
# makeCacheMatrix is a function that creates a list of functions providing
#                 information regarding the Inverse of the matrix input parameter
#
#   setCachedMatrix  * caches the matrix input parameter and sets the values of
#                      the inverse matrix to null(caches this value)
#   getCachedMatrix  * returns the value of the cached value set in setCachedMatrix
#   setCachedInverse * caches the inverse of the matrix input parameter
#   getCachedInverse * returns the values of the cached inverse matrix
#                      set in setCachedInverse
#
# 
# Required input parameters
#   matrix_in  is a  matrix variable
##############################################################################
makeCacheMatrix <- function(matrix_in = matrix()) {
  cachedMatrix_v  <- matrix_in
  cachedInverse_v <- NULL
  setCachedMatrix <- function() {
    cachedMatrix_v  <<- cachedMatrix_v
    cachedInverse_v <<- NULL
  }
  # Return Cached Matrix
  getCachedMatrix  <- function() cachedMatrix_v

  # Set Inverse of Cached Matrix
  setCachedInverse <- function() cachedInverse_v <<- solve(cachedMatrix_v)

  # Return Inverse of Cached Matrix
  getCachedInverse <- function() cachedInverse_v

  # Return of List of functions regarding cached values
  list(getCachedMatrix = getCachedMatrix,
       setCachedMatrix = setCachedMatrix,
       getCachedInverse = getCachedInverse,
       setCachedInverse = setCachedInverse
       )
}


##############################################################################
# cacheSolve *This function computes the inverse of the special "matrix" returned 
#             by makeCacheMatrix above. If the inverse has already been calculated, 
#             then cacheSolve should retrieve the inverse from the cache.
#
# Required input parameters
#   x is a list variable create by makeCacheMatrix function
##############################################################################
cacheSolve <- function(x, ...) {
  #Get value of cached matrix
  cachedInverseMatrix <- x$getCachedInverse()
  
  #Check if Cached Inverse Matrix is null
  if(!is.null(cachedInverseMatrix)  ) {
    # Message to information user that cached value was used
    message("Retrieve cached inverse matrix")
    return(cachedInverseMatrix)
  }
  
  #If cached inverse matrix was null (not set using setCachedInverse function) then
  #set and return the inverse of the cached matrix
  cachedInverseMatrix <- x$setCachedInverse()
  return(cachedInverseMatrix) 
}
