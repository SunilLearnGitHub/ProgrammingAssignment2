makeCacheMatrix <- function(inputMatrix) {
  
  # I have commented this code for peer reviewers...
  
  # create an null cachedMatrix object, this will 
  # eventually hold the cached matrix. Null will be
  # initial state of the cachedMatrix object. Later on, 
  # cachedMatrix will checked for null, it it is null, 
  # that will mean the input matrix is a new matrix that
  # needs to be inverted and cached.
  cachedMatrix <- NULL
  
  # setMatrix function will be used to reset the input matrix with the 
  # new matrix passed in by the user. Since we have a new matrix passed 
  # in by the user, the new matrix has not been cached yet, therefore we
  # also to reset the cached matrix
  setMatrix <- function(newMatrixFromUser) {
    inputMatrix <<- newMatrixFromUser
    cachedMatrix <<- NULL
  }
  
  # getMatrix function will be used to return the new (non inverted) matrix 
  # passed in by the user 
  getMatrix <- function() {
    return(inputMatrix)
  }
  
  # setCache will be called by the cacheSolve function after it does a solve calculation, 
  # this will cache resulting inverted matrix from the cacheSolve function
  setCache <- function(newCache) {
    cachedMatrix <<- newCache
  }
  
  # getCache will return the cached matrix
  getCache <- function() {
    return(cachedMatrix)
  }
  
  list (
    setMatrix = setMatrix, 
    getMatrix = getMatrix,
    setCache = setCache,
    getCache = getCache
  ) 
}

cacheSolve <- function(inputMatrix, ...) {
  
  # get the cached matrix
  cachedMatrix <- inputMatrix$getCache()
  
  # if the cached matrix is not null, that means 
  # there is a matrix that is cached, so return the 
  # cached matrix, because the cached matrix has already
  # be inverted (solve calculation has been applied to the
  # matrix) 
  if(!is.null(cachedMatrix))
  { 
    # return cached matrix 
    return(cachedMatrix)
  }
  else
  {
    # the input matrix is a new matrix, it has not been
    # inverted (no solve calc. applied to it) and it has not been 
    # cached, so do those operations and then return the inverted matrix
    matrix <- inputMatrix$getMatrix()
    invertedMatrix <- solve(matrix, ...)
    inputMatrix$setCache(invertedMatrix)
    return(invertedMatrix)
  }
}
