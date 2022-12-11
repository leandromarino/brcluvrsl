PCA_sdev_check <- function(data){
  pca <- data %>% stats::prcomp()
  sdev <- pca$sdev %>% prop.table()
  return(sdev)
}


PCA_weight <- function(
    data,
    threshold = NULL
){

  if(is.null(threshold)){
    threshold <- 0
    warning("'threshold' parameter not defined. Using all principal components in weighting.")
  }

  pca <- data %>% stats::prcomp()
  pca_sdevprop <- pca$sdev %>% prop.table()
  pca_loadings <- pca$rotation

  pca_loadings %>%
    apply(., 1, function(x, sdev = pca_sdevprop, crit = threshold){
      aux <- sdev >= crit
      abs(x[aux] * sdev[aux])
    }) %>%
    colSums() -> PCA

  PCA / max(PCA)

  PCA
}
