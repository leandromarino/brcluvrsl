RanD <- function(u){

  varsel <- rep(FALSE, length(u))
  while(sum(varsel) == 0){
    varsel <- u > runif(1)
  }
  varsel

}


