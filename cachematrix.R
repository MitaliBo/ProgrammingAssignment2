## There are two functions that cache the inverse of matrix.Matrix converion is
##usually a costly computation so it is better to cache.The makeCacheMatrix() 
##function creates a special matrix object that can cache its inverse and 
##cacheSolve computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the 
##matrix has not changed), then the cacheSolve() should retrieve the inverse 
##from the cache.


##This function has getters and setters for the input matrix and the inverse of
##input matrix like get,set and getInverse,setInverse respectively .These will 
##be used by cacheSolve() function below.


makeCacheMatrix <- function(x = matrix()) {

  inv_mat<-NULL
  set <-function(y)
    {
       x<<-y
      inv_mat<<-NULL
   }
  
  get<-function()x
  setInverse<- function(inverse) inv_mat<<-inverse
  getInverse<-function() inv_mat
  
  list(set=set,get=get,
       setInverse=setInverse,
       getInverse=getInverse)
  
  
}

## This function will compute the inverse of the input matrix using predefined 
##function solve() if its not been caclculated before else it  will return from 
##the cache.


cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv_mat<- x$getInverse()
  if(!is.null(inv_mat))
    
  {
    message("getting cached inverse matrix.....")
    return(inv_mat)
  }
  
  mat<-x$get()
  inv_mat<-solve(mat,...)
  x$setInverse(inv_mat)
  inv_mat

}
