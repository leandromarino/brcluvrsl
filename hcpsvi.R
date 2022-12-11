hcpsvi <- function(df,
                          kmin = 2, kmax = NULL,
                          type = 'none',
                          sample_size = 250,
                          metric = 'euclidean',
                          ...){

  start_time = Sys.time()

  # metodo nenhum metodo - ou seja sem selecao
  if(type == 'none'){
    .varsel <- rep(1, ncol(df)) == 1
  }


  # metodo aleatorio
  if(type == 'random'){
    lapply(as.list(1:sample_size), function(x, numcol = ncol(df)){
      VARSEL <- rep(FALSE, numcol)
      while(sum(VARSEL) == 0){ # evitar que o vetor sem variaveis seja selecionado
        VARSEL <- (runif(numcol) > runif(1))
      }
      return(VARSEL)
    }) %>% do.call(rbind, .) %>%
      magrittr::set_colnames(colnames(df))
  } -> .varsel

  .all_k <- as.list(kmin:kmax)


  lapply(as.list(1:sample_size), function(x){
    process_start_time <- Sys.time()
    .dissimilarity <- cluster::daisy(df[, .varsel[x, ], drop = FALSE], metric = metric)
    .partition <- lapply(.all_k, function(xx){
      cat('K-medoids Clustering, id = ', x, 'k = ', xx, '\n')
      cluster::pam(x = .dissimilarity, k = xx)
    })
    names(.partition) <- paste0('k_', kmin:kmax)
    cat('Obtaining Silhouettes', '\n')

    .silhouette <- .partition %>%
      lapply(., function(x)
        data.frame(sil_mean = mean(x$silinfo$widths[, 'sil_width' ]),
                   sil_sd = sd(x$silinfo$widths[, 'sil_width' ]),
                   sil_q10 = quantile(x$silinfo$widths[, 'sil_width' ], .10),
                   sil_q25 = quantile(x$silinfo$widths[, 'sil_width' ], .25),
                   sil_q50 = quantile(x$silinfo$widths[, 'sil_width' ], .50),
                   sil_q75 = quantile(x$silinfo$widths[, 'sil_width' ], .75),
                   sil_q90 = quantile(x$silinfo$widths[, 'sil_width' ], .90)))  %>%
      do.call(rbind, .) %>%
      dplyr::mutate(sample_id = x,
                    k = kmin:kmax,
                    nvars_selec = sum(.varsel[x, ]),
                    vars_selec = colnames(.varsel)[.varsel[x, , drop = FALSE] == 1] %>%
                      paste0(., collapse = ', ')) %>%
      dplyr::relocate(sample_id, k, nvars_selec, vars_selec)


    saida <- list(partition = .partition, silhouette = .silhouette,  subpro_start = process_start_time, subpro_end = Sys.time())
    return(saida)
  }) -> result
  names(result) <- paste0('id', 1:sample_size)

  result <- list(saida = result, start_time = start_time, end_time = Sys.time())
  return(result)
}
