brcluvrsl <- function(Data,
                  Fo, Fo_p1 = NULL, Fo_p2 = NULL,
                  Dc, Dc_p1 = NULL, Dc_p2 = NULL, Dc_p3 = NULL,
                  MAX = FALSE,
                  n,
                  rc = 0.7,
                  pe = 0.2, pm = 0.2, p = 100,
                  ng = 2000, ngw = 500, MaxTime = 3600)
{
  # browser()
  # adaptado de brkga::brkga

  cpu_time <- proc.time()
  cl <- match.call()

  time_iter <- 0
  f <- BRKGA::popgen(n,p)


  # ------------------------------------------ !
  # ------------------------------------------ !
  # aplicando o decodificador utilizando os parâmetros externos ####
  # ------------------------------------------ !
  if(is.null(Dc_p1) == TRUE)
  {
    g <- t(apply(f, 1, function(x) Dc(x)))
  } else {
    if(is.null(Dc_p2) == TRUE)
    {
      g <- t(apply(f,1,function(x) Dc(x, Dc_p1)))
    } else {
      if(!is.null(Dc_p2) & is.null(Dc_p3))
      {
        g <- t(apply(f,1,function(x) Dc(x, Dc_p1, Dc_p2)))
      } else
      {
        g <- t(apply(f,1,function(x) Dc(x, Dc_p1, Dc_p2, Dc_p3)))
      }
    }
  }



  if(nrow(g)==1) g <- t(g)

  cat(dim(g))





  # ------------------------------------------ !
  # ------------------------------------------ !
  # aplicando os parâmetros externos na funçào objetivo ####
  # ------------------------------------------ !

  if(is.null(Fo_p1) == TRUE)
  {
    ft <- apply(g, 1, function(x) Fo(Data, x))
  } else {
    if(is.null(Fo_p2) == TRUE)
    {
      ft <- apply(g, 1, function(x) Fo(Data, x, Fo_p1))

    } else {
      ft <- apply(g, 1, function(x) Fo(Data, x, Fo_p1, Fo_p2))
    }
  }


  fbest <- ifelse(MAX == TRUE, -Inf, Inf)
  i <- 0
  pelite <- round(pe * p)
  pmutant <- round(pm * p)
  ngwb <- 0

  # -----------------------------------  !
  # -----------------------------------  !
  # # iniciando o BRKGA ####
  # -----------------------------------  !
  while((i < ng) & (ngwb <= ngw) & (time_iter < MaxTime))
  {
    i <- i + 1
    iter_ini <- Sys.time()


    ngwb <- ngwb + 1
    pq <- order(ft, decreasing = MAX)
    f <- f[pq,] #Sorting by Fitness
    g <- g[pq,]
    if(is.matrix(g) == FALSE) g <- as.matrix(g)
    fmin <- ft[pq[1]]
    ft <- ft[pq]

    if (MAX == FALSE)
    {
      if (fmin < fbest)
      {
        fbest <- fmin
        gbest <- g[1,]
        solution_best <- c(fbest, gbest)
        cat("Best Solution Generation ",i," = ",fbest,"\n")
        flush.console()
        ibest <- i
        ngwb <- 0
      }
    } else {

      if (fmin > fbest)
      {
        fbest <- fmin
        gbest <- g[1,]
        solution_best <- c(fbest, gbest)
        cat("Best Solution Generation ",i," = ",fbest,"\n")
        flush.console()
        ibest <- i
        ngwb <- 0
      }

    }
    felite <- f[1:pelite,]
    fnonelite <- f[(pelite+1):p,] #Non-Elite
    fmutant <- BRKGA::popgen(n,pmutant)
    fnovos <- BRKGA::crossover(felite,fnonelite,rc,p,pelite,pmutant,n)
    fnew <- rbind(fmutant,fnovos)

    if(is.null(Dc_p1) == TRUE)
    {
      gnew <- t(apply(fnew, 1, function(x) Dc(x)))
    } else {
      if(is.null(Dc_p2) == TRUE)
      {
        gnew <- t(apply(fnew,1,function(x) Dc(x, Dc_p1)))
      } else if(!is.null(Dc_p2) & is.null(Dc_p3)){
        gnew <- t(apply(fnew,1,function(x) Dc(x, Dc_p1, Dc_p2)))
      } else {
        gnew <- t(apply(fnew,1,function(x) Dc(x, Dc_p1, Dc_p2, Dc_p3)))
      }
    }
    if ( nrow(gnew) == 1 ) gnew <- t(gnew)

    if ( ncol(g) == 1 )
    {
      g <- rbind(as.matrix(g[1:pelite,]), gnew)
    } else {
      g <- rbind(g[1:pelite,], gnew)
    }

    f <- rbind(felite, fnew)
    glk <- g[(pelite+1):p,]

    if (is.matrix(glk) == FALSE) glk <- as.matrix(glk)
    if (is.null(Fo_p1)==TRUE)
    {
      ftk <- apply(glk, 1, function(x) Fo(Data, x))
    } else {
      if (is.null(Fo_p2)==TRUE)
      {
        ftk <- apply(glk, 1, function(x) Fo(Data, x, Fo_p1))
      } else {
        ftk <- apply(glk, 1, function(x) Fo(Data, x, Fo_p1, Fo_p2))
      }
    }

    ft <- c(ft[1:pelite],ftk)

    iter_fim <- Sys.time()
    cat('\rGeneration: ',i, 'elapse_time: ', difftime(iter_fim, iter_ini, units = 'secs'))
    time_iter <- (proc.time() - cpu_time)[3]
  }
  cpu_time <- (proc.time() - cpu_time)[3]

  # saida da funcao ####
  return(list(fbest = fbest,
              gbest = gbest,
              cpu_time = cpu_time,
              ibest = ibest,
              i_actual = i,
              ngw_actual = ngwb,
              call = cl))
}


