#This function creates a random "matrix" 
#object that can cache its inverse.

makeCacheMatrix <- function(MyDim, ...) {
  MyDim<-as.numeric(MyDim)
  if(MyDim<=1) {
    print("error, argument must be greater than 1")
    exit()
  } else {
    set.seed(123)
    RndVec <- rnorm(MyDim)
    RndMat <- matrix(rexp(MyDim*MyDim), MyDim)
  
    MyDet <-  det(RndMat) #!= 0  #determinant of RndMat
  
    if(MyDet==0) {
      print("error, matrix not invertible, retry")
      exit()
      } else {
      RndMat <<- RndMat # RndMat
      MyInv <<- solve(RndMat) #inverse of RndMat
    }

    print('Matrix is RndMat =')
    print(list(RndMat))
    print('Inverse is MyInv =')
    print(list(MyInv))

  }
}

#Computing the inverse of a square matrix with solve function 
#cacheSolve function computes the inverse of the special 
#"matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated 
#(and the matrix has not changed), then cacheSolve should retrieve 
#the inverse from the cache.

#we pass the random matrix (x=RndMat) and its inverse (y=MyInv)
#z is another random matrix computed in this function
#if z and x are identical the function retrieves the inverse 
#y already calculated, otherwise a new inverse is calculated

cacheSolve <- function(x,y, ...) {
  MyDim <- nrow(y)
  set.seed(123)
  RndVec <- rnorm(MyDim)
  z <- matrix(rexp(MyDim*MyDim), MyDim)

  if(identical(x,z)) {
    message("getting cached data for inverse, MyInv =")
    return(MyInv)
  }
  NewMyInv <<- solve(z)
  print('New inverse is NewMyInv =')
  print(list(NewMyInv))

}
