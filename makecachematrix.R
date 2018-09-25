makeCacheMatrix<-function(mat=matrix()){
    #mat<-matrix()
    invmat<-NULL
    setmatrix<-function(m){mat<<-m ; invmat<<-NULL}
    getmatrix<-function(){mat}
    setinversematrix<-function(invm){invmat<<-invm}
    getinversematrix<-function(){invmat}
    funcmatrixlist=list(setmatrix=setmatrix,getmatrix=getmatrix,setinversematrix=setinversematrix,getinversematrix=getinversematrix)
    
    funcmatrixlist
    
}
cacheSolve<-function(mat,...){invmat<-mat$getinversematrix()
if(!is.null(invmat)){print("cached data");return(invmat)}
matr <- mat$getmatrix()
invmat <- solve(matr, ...)
mat$setinverse(invmat)
print("Non ached data")
invmat
}