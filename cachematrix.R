################ R Programming: Assignment 2 #######################
## The functions below illustrate the concept of 
## "caching" relevant information about a data,
## if it is costly to compute the information every time.
## The first function creates a placeholder for "the data" 
## of interest which is a matrix and also a placeholder
## for it's inverse. Further it returns a list of functions
## to set and get the elements of the data matrix and it's inverse.
## The second function shows how the getinv and setinv functions
## can be used to decide if the inverse can be returned as is or
## needs to be re-computed.
## Overall this also illustrates the concpet of OOP.
## Both the data matrix and it's inverse are 'hidden' inside 
## the scope of the first function and can only be accessed
## by the 'methods' exposed - the get and set functions only.

####################################################################
## Function: makeCacheMatrix
## Descr: Creates a matrix and returns a list
##        containing functions to set and get the
##        matrix and it's inverse.
##        The inverse is initialized and stored,
##        for future reference.
#####################################################
makeCacheMatrix <- function(m = matrix()) {
  
  #Initialize the inverse of matrix to NULL
  #m_inv is the "cached" variable.
  m_inv <- matrix()
  m_inv <- NULL
  
  #Function to set the values of the matrix
  set <- function(y) {
    m <<- y
    m_inv <<- NULL
  }
  
  #Function to get the values of the matrix
  get <- function(){ m }
  
  #Function to set/store the inverse computed elsewhere
  setinv <- function(inv) { m_inv <<- inv }
  
  #Function to get the inverse matrix
  getinv <- function() { m_inv }
  
  # Return a list containing the above four functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#####################################################
## Function: cacheSolve
## Descr: Uses the functions exported by makeCacheMatrix
##        to check if inverse is already computed, and
##       if it is so the stored value is returned.
##       if it is not computed then the new inv is computed and
##        (a) stored for future reference and (b) returned.
##        
#####################################################

cacheSolve <- function(x, ...) {
  
  #Get the stored value
  m_inv <- x$getinv()
  
  # If the value is not NULL, it means matrix data has not changed
  # and inverse has been computed and stored earlier.
  if(!is.null(m_inv)) {
    message("Returning the cached inverse")
    return(m_inv)
  }
  
  # If the function execution arrives here, means inverse was NULL,
  # implies it was not computed earlier after the data matrix was 
  # been freshly initialized..
  
  # Get the matrix (having new values)
  data <- x$get()
  # Compute the new inverse
  m_inv <- solve(data, ...)
  # Store the inverse for future reference
  x$setinv(m_inv)
  # Return the inverse matrix
  m_inv
}