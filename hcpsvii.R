hcpsvii <- function(data, k = NULL, kmin = 2, kmax = NULL, psv,
                           metric, std_method = 'none', sample_size,
                           debug = FALSE){

  #     data = sales[, -1]
  #     k = NULL
  #     kmin = 2
  #     kmax = 3
  #     psv = 0.5
  #     std_method = 'none'
  #     metric = 'gower'
  #     sample_size = 10
  #     debug = FALSE

  output <- list(call        = match.call(),
                 data_orig   = NULL,
                 data_std    = NULL,
                 result      = NULL,
                 start_time  = Sys.time(),
                 finish_time = NULL)

  if(debug) browser()

  # if kmax not provided kmax as Kaufmann
  if(is.null(kmax)){kmax <- data %>% nrow() %>% sqrt() %>% ceiling()}

  # obtaining max number of variables
  p_max <- data %>% ncol

  # standardizing the dataset
  output$data_orig <- data
  output$data_std <- std_data(data = data, std_method = std_method)
  rm(data)

  if(!is.null(k)) kmin <- kmax <- k

  result <- list()


  ii = 1
  for(k_atual in kmin:kmax){  ## loop ii
    cat('k = ', k_atual, '\n')
    result[[ii]] <- list()
    jj = 1
    for(aux_psv in psv){
      result[[ii]][[jj]] <- list()
      cat('k = ', k_atual,' - psv = ', aux_psv, '\n')
      tmp_start_time = Sys.time()
      ## defining number of selected variables
      (ite_p <- (aux_psv * p_max) %>% ceiling())
      # max of combinations
      max_combn <- choose(p_max, ite_p)
      ## effective sample_size
      if(max_combn > sample_size) ss_ef <- sample_size else ss_ef <- max_combn
      ## combinatorial sampling
      var_selec <- RcppAlgos::comboSample(v = p_max, m = ite_p, repetition = FALSE, n = ss_ef) %>%
        as.data.frame() %>%
        tibble::add_column(sample_id = 1:nrow(.), .before = 'V1')


      cl <- makeCluster(8L)

      result[[ii]][[jj]] <- pbapply::pbapply(var_selec, 1, function(z){
        sample_id <- unlist(z)[1]

        nm_var_selec <- paste0('id', sample_id)

        vars <- unlist(z)[-1]


        tmp_data <- output$data_std[, vars, drop = FALSE]

        # calculating the dissimilarity matrix with the euclidean metric
        diss_matrix <- cluster::daisy(x = tmp_data, metric = metric, stand = FALSE)

        #defining kmeans as the default with 10 different starts
        clustering <- cluster::pam(x = diss_matrix, k = k_atual)

        # indices de ajuste para o psv
        fit_silhouette <- cluster::silhouette(x = clustering$clustering, dist = diss_matrix)
        fit_calinski_harabasz <- fpc::cluster.stats(d = diss_matrix, clustering = clustering$clustering)

        (obj_name <- paste0('wine_kmeans',
                            '_psv',sprintf('%03d', as.integer(round(psv * 100))),
                            '_ss', sprintf("%04d", sample_size),
                            '_k', sprintf('%02d', k_atual),
                            '_id', sprintf("%04d", sample_id)))

        sil_stats <- function(x){
          c('mean' = mean(x),
            'sd'   = sd(x),
            'q10'  = quantile(x, 0.10, names = FALSE),
            'q50'  = quantile(x, 0.50, names = FALSE))
        }

        return(list(k = k_atual,
                    psv = psv,
                    sample_id = sample_id,
                    vars = vars,
                    clustering = clustering,
                    fit_silhouette = fit_silhouette,
                    fit_silhouette_stats = sil_stats(fit_silhouette[,3]),
                    fit_calinski_harabasz = fit_calinski_harabasz,
                    start_time = tmp_start_time,
                    end_time = Sys.time()))
      }, cl = cl)

      stopCluster(cl)
      jj <- jj + 1
    }
    ii <- ii + 1
  }

  output$finish_time <- Sys.time()
  output$result <- result
  return(output)

}
