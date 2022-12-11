
fobj_silmed <- function(D, x, k, metric){

  lapply(as.list(k), function(xx){
    # calculando a matriz de dissimilaridade
    dissimilarity <- cluster::daisy(D[, x, drop = FALSE], metric = metric)
    partition <- cluster::pam(x = dissimilarity, k = xx)
    fo <- partition$silinfo$widths[, 'sil_width' ] %>% mean()

    result <- list(diss = dissimilarity, partition = partition, fo = fo, k = xx)
    return(result)
  }) -> auto_cluster

  best_sol <- auto_cluster %>% lapply(., function(X){X$fo}) %>% unlist() %>% match(x = max(.), .)

  auto_cluster <- auto_cluster[[best_sol]]

  # não precisa exportar o valor 'k'de grupos pois pode ser obtido a posteriori.
  #
  result <- auto_cluster[c('fo')][[1]]

  result
}

