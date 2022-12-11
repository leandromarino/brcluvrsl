
WeiD <- function(u, weight, nminvar = 5){

  # for testing purposes
  # u <- runif(10)
  # weight <- exp(runif(10))
  # nminvar <- 4

  if(length(u) != length(weight)){
    stop('Decoder: u and weight has different sizes')
  }

  weight <- weight/max(weight)

  weight[order(weight, decreasing = TRUE)[1:nminvar]] <- 1

  varsel <- u <=s weight

  return(varsel)

}
