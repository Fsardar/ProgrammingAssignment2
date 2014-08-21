## creates a list object with functions to set and get the matrix & Inverse
## assuming matrix passed is invertable 

makeCacheMatrix <- function(x = matrix()) {
  CacheInverse <<-NULL ## sets CacheInverse as NULL - Flag to check for a cached matrix
  
  SetCacheM<- function(y){
      CacheMatrix <<- y ##caches value of matrix
      CacheInverse <<-NULL ##resets to cache to NULL - Flag reset when loading new matrix
  }
  
  GetCacheM<-function() {
    CacheMatrix
  }
  
  SetInvM<-function(InverseMatrix){
    CacheInverse<<-InverseMatrix
  }
  GetInvM<-function(){
    CacheInverse
  }
  
  list(SetCacheM=SetCacheM, GetCacheM=GetCacheM, SetInvM=SetInvM, GetInvM=GetInvM)
  
}

  ## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    m<-x$GetInvM()
    
    if(!is.null(m)){ ## test for cached value - if available, it retrieves cache
      message("Getting Cached Data")
      return (m)
    }
    
    NewMatrix <- x$GetCacheM()
    Inverse <- solve(NewMatrix) ## solves invertible matrix
    x$SetInvM(Inverse) 
    message ("Setting New Inverse Matrix")
}