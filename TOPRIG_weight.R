

TOPRIG_weight <- function(data, metric, k){

  cat('1. obtaining the best number of groups k\n')
  lapply(as.list(k), function(xx){
    cluster::daisy(data, metric = metric) %>%
      cluster::pam(x = ., k = xx) %>%
      .$silinfo %>%
      .$avg.width
  }) %>% unlist() %>%
    data.frame(k = k, sil_med = .) %>%
    dplyr::arrange(dplyr::desc(sil_med)) %>%
    head(n=1) %>%
    .$k -> best_k


  cat('2. obtaining the clustering groups for each variable IVC (indidual-variable-clustering)\n')
  data %>% as.list() %>%
    lapply(., function(obj, Metric = metric, k = best_k){
      cluster::daisy(x = obj %>% as.data.frame, metric = Metric) %>%
        cluster::pam(x = ., k = best_k) %>%
        .$cluster
    }) %>%
    as.data.frame -> ivc



  cat('3. computing the ARI matrix between all IVC\n')
  combn(colnames(ivc), 2) %>%
    apply(., 2, function(x, data = ivc){
      mclust::adjustedRandIndex(x = data[, x[1]], y = data[,x[2]])
    }) -> parwise_ari

  cat('4. filling the upper triangle of the matrix\n')
  m_ari <- matrix(0, nrow = ncol(ivc), ncol = ncol(ivc))
  m_ari[upper.tri(x = m_ari, diag = FALSE)] <- parwise_ari

  cat('5. filling the lower triangle of the matrix (symmetric matrix)\n')
  order_mat <- which(lower.tri(m_ari, diag = FALSE), arr.ind = TRUE)
  m_ari[order_mat[order(order_mat[, 1]), ]]  <- parwise_ari

  cat('6. removing the influence of negative values\n')
  m_ari[m_ari < 0] <- 0

  TOPRIG <- m_ari %>% rowSums()

  TOPRIG <- TOPRIG / max(TOPRIG)

  TOPRIG


}
