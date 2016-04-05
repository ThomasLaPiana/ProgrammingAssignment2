makeCacheMatrix <- function(x = matrix()){
    m=NULL
    
    set=function(y){
      x<<-y
      m<<-NULL}
    
    get = function() {return(x)}
    setInv = function(i) {m<<-i}
    getInv = function() {return(m)}
    
    out = list(
      set=set,get=get,
      setInv=setInv,getInv=getInv)
    
    return(out)
}

cacheSolve <- function(x, ...){
  
    inv=x$getInv()
    
    if(!is.null(inv)) {return(inv)}
    
    data=x$get()
    inv=solve(data,...)
    x$setInv(inv)
    return(inv)
}