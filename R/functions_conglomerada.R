conglomerada = function(df_alunos, n){

    real_log_port = df_alunos |>
      dplyr::pull(log_port) |>
      mean()
    
    set.seed(123)
    IACS =
      list("escola", "turma", c("escola", "turma"), c("escola", "turma", "aluno")) |>
      map(~replicate(1000, select_from_n_cluster(df_alunos, n = n, .)))  |>
      setNames(c("escola", "turma", paste("escola", "turma", sep = "_"), paste(c("escola", "turma", "aluno"), collapse = "_")))
    
    medias = 
      IACS |>
      map2(
        .y = names(IACS),
        function(.x, .y){
          .x[nrow(.x)-1,] |>
            lapply( 
              function(x){
                data = sampling::getdata(df_alunos, x) 
                plan = 
                  survey::svydesign(data = data, ids =as.formula(paste("~", gsub("_", "+", .y))), probs=~x$Prob)
                  survey::svymean(~log_port,plan) |>
                  as.data.frame()
              }
             )
        }
      ) |>
      map(
        ~.x |>
          dplyr::bind_rows() |>
          dplyr::rename(Estimativas = "mean", `ErroPadrão` = "log_port") |>
          dplyr::as_tibble() |>
          dplyr::mutate(
            Estimativas = exp(Estimativas)
          ) |>
          dplyr::mutate(
            IntContReal = Estimativas-1.96*`ErroPadrão` < exp(real_log_port) & exp(real_log_port) < Estimativas+1.96*`ErroPadrão`,
            Vies = Estimativas-exp(real_log_port)
          ) |>
          dplyr::mutate(
            EQM = (Vies**2)
          ) |>
          dplyr::summarise_all(mean)
          ) |>
      dplyr::bind_rows()


    return(medias)
}

conglomerada_ppt = function(df_alunos, n){

  real_log_port = df_alunos |>
    dplyr::pull(log_port) |>
    mean()

  set.seed(065)
  IACS_ppt =
      mapply(
        function(var, tam){ 
          replicate(1000, select_from_n_cluster(df_alunos, n = n, var = var, ppt = T, tam = tam))
           },
        var = list("escola", "escola"),
        tam =  list("turma", "alunos"),
        SIMPLIFY = F
      ) |>
      setNames(c("escola", "escola"))

  medias = 
      IACS_ppt |>
      map2(
        .y = names(IACS_ppt),
        function(.x, .y){
          .x[nrow(.x)-1,] |>
            lapply( 
              function(x){
                data = sampling::getdata(df_alunos |> dplyr::mutate(alunos = 1:n()), x) 
                plan = survey::svydesign(data = data, ids = as.formula(paste("~", gsub("_", "+", .y))), probs=~x$Prob, fpc = rep(n, dim(x)[1]))
                survey::svymean(~log_port,plan) |>
                  as.data.frame()
              }
             )
        }
      ) |>
      map(
        ~.x |>
          dplyr::bind_rows() |>
          dplyr::rename(Estimativas = "mean", `ErroPadrão` = "log_port") |>
          dplyr::as_tibble() |>
          dplyr::mutate(
            IntContReal = mean(Estimativas-1.96*`ErroPadrão` < real_log_port & real_log_port < Estimativas+1.96*`ErroPadrão`),
            Vies = Estimativas-real_log_port
          ) |>
          dplyr::mutate(
            EQM = mean(Vies**2)
          ) |>
          dplyr::summarise_all(mean)) |>
      dplyr::bind_rows()

  return(medias)
}

conglomerada_500 = function(df_alunos){
   conglomerada(df_alunos, 500)
}
conglomerada_750 = function(df_alunos){
   conglomerada(df_alunos, 750)
}
conglomerada_ppt_500 = function(df_alunos){
   conglomerada_ppt(df_alunos, 500)
}
conglomerada_ppt_750 = function(df_alunos){
   conglomerada_ppt(df_alunos, 750)
}