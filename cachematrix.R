
##Catching the inverse of a matrix 
##makeCacheMatrix: This function creates a special "matrix" 
##object that can cache its inverse.

makeCacheMatrix <- function(mat=matrix()){
    inv<- NULL 
    
    ##getting the matrix
    get <- function() mat
    
    ##setting the matrix
    set <- function(matrixx){
        mat <<- matrixx
        inv<<- NULL 
    }
    
    ##get the inverse
    getinv <- function() inv
    
    ##set the inverse
    setinv <- function(inverseMat) inv<<-inverseMat
    
    return(list(get=get,set=set,getinv=getinv,setinv=setinv))
}


##cacheSolve: This function computes the inverse of the special
##"matrix" returned by makeCacheMatrix above. If the inverse 
##has already been calculated (and the matrix has not changed)
##then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(mat,...){
  
  inv <- mat$getinv()
    if(!is.null(inv) && is.matrix(inv)){
      message('getting cached data')
      return(inv)
    }
  
  matToSolve<-mat$get()
  inv<-solve(matToSolve,...)
  mat$setinv(inv)
  inv
  
  
}
