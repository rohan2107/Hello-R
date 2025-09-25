myFirstRFunc <- function(n){

  stopifnot(is.numeric(n), n%%1==0, n>=0)
  
  sum <-0
  
  for(i in 2:n-1){
    if(i%%2==0 || i%%7==0){
      sum <- sum+i
    }
  }
  
  return(sum)
  
}