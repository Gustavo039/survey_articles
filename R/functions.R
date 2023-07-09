read_file = function(file){
  df_alunos = read.table('https://raw.githubusercontent.com/Gustavo039/survey_articles/main/Alunos.txt', header = T) |> # nolint: line_length_linter.
    mutate(log_port = log(port), .keep = 'unused')
  return(df_alunos)
}

aes_unif_size = function(n_size, n_strata){
  ret = ((rep(1/n_strata,n_strata) * n_size) |> ceiling() )
  return(ret)
}

neyman_calc = function(data, n) {
  num = length(data) * sd(data) 
  return(as.data.frame(num))
}

AES_estimates = function(df_data, strat_name, rep_size, sample_size, sample_size_type){

   real_log_port = df_data |>
     dplyr::pull(log_port) |>
     mean()

  if(sample_size_type == 'uniform'){
    strata_size = df_data |> select(all_of(strat_name)) |> n_distinct()
    strata_size = aes_unif_size(sample_size, strata_size)
  }
  else 
    if(sample_size_type == 'proportional'){
      if(strat_name == 'escola'){
           strata_size =  df_data |>
                          group_by(escola) |>
                          reframe(group_size = (n()/(df_data |> nrow())) |>
                                    {\(x) x * sample_size}() |>
                                    ceiling())|>
                          {\(x) x$group_size}()
      }
      else
        if(strat_name == 'rede'){
           strata_size =  df_data |>
                          group_by(rede) |>
                          reframe(group_size = (n()/(df_data |> nrow())) |>
                                    {\(x) x * sample_size}() |>
                                    ceiling())|>
                          {\(x) x$group_size}()
      }
    }
  else{
    if(strat_name == 'escola'){
       strata_size = 
         df_data |>
         group_by(escola)

       strata_size = 
         strata_size |>
         group_modify(~ neyman_calc(data = .x$log_port,n = n_size)) |>
         ungroup() |>
         mutate(deno = sum(num)) |>
         mutate(group_size = ((num/deno)*sample_size) |> ceiling())|>
        {\(x) x$group_size}()
    }
    else
      if(strat_name == 'rede'){
       strata_size = 
                      df_data |>
                      group_by(rede) 
                    
                    strata_size = strata_size |>
                      group_modify(~ neyman_calc(data = .x$log_port,n = sample_size)) |>
                      ungroup() |>
                      mutate(deno = sum(num)) |>
                      mutate(group_size = ((num/deno)*sample_size) |> ceiling())|>
                          {\(x) x$group_size}()
    }
  }


  IAESs = replicate(n = rep_size, 
                  expr = sampling::strata(df_data,
                       stratanames = strat_name,
                       size = strata_size,
                       method = 'srswor') |>
                    {\(x) data.frame(x$ID_unit, x$Prob)}()
  )

  AESs = sapply(1:rep_size, function(i) {
    df_data |>
    filter(aluno %in% IAESs[,i]$x.ID_unit) 
    }
  )

  fpc_calc = function(N,n){
    ret = (((N-n)/(N-1))**(1/2))
    return(ret)
  }

  AESs_estimates = sapply(1:rep_size,function(i){
    plan = svydesign(~1, strata=~rede, data = AESs[,i] |> as.data.frame(), probs=~IAESs[,i]$x.Prob)
    svymean(~log_port,plan) |>
    as.data.frame()
  }
  )


  ret = data.frame('Estimativas' = AESs_estimates[1,] |> unlist(), 
                   'ErroPadrão' = AESs_estimates[2,] |> unlist()) |>
    mutate('IntContReal' = ifelse(real_log_port > Estimativas - 1.96 * ErroPadrão & real_log_port < Estimativas + 1.96 * ErroPadrão,
                                  1, 0)
           )

  return(ret)
}

aess_table = function(data, real_log_port){
  ret = data |>
    modify(mean) |>
    slice(1) |>
    mutate(Vies = Estimativas - real_log_port) |>
    mutate(EQM = Vies^2 + ErroPadrão^2) 
  
  return(ret)
}

select_from_n_cluster = function(df_alunos, n, var, ppt = F, tam = NULL){
  
  n_vars = length(var)

  media_escola = df_alunos |>
    count(escola) |>
    pull(n) |>
    mean()

  media_turma = df_alunos |>
    count(turma) |>
    pull(n) |>
    mean()


  f = ifelse(
    n_vars == 1, 
    ifelse(
      ppt,
      function(x){
        sampling::cluster(
          x,
          clustername = var,
          size = 1,
          method = c("poisson"),
          pik =  sampling::inclusionprobabilities(
            x |>
              mutate(alunos = 1:n()) |>
              group_by_at(var) |>
              summarise_at(tam, ~length(unique(.))) |>
              pull(tam), 1
          )
        ) |>
          list()
      },
      function(x){
        sampling::cluster(
          x,
          clustername = var,
          size = 1,
          method = c("srswor")
        ) |>
          list()
      }), 
    function(x){
      sampling::mstage(
        x, 
        stage = rep("cluster", n_vars),
        varnames = var,
        size = lapply(rep(1, n_vars), function(x) x),
        method = rep("srswor", n_vars)
      )
    }
  )
  indexes = f(df_alunos)
  i = 0
  while(i < n){
    indexes_new = f(df_alunos[-indexes[[length(indexes)]]$ID_unit,])
    if(is.null(indexes_new[[1]])) next
    
    indexes = map2(
      indexes, 
      indexes_new, 
      function(x, y) rbind(x, y)
    )
    
    i = nrow(indexes[[length(indexes)]]) 
  }
  
  indexes$n = i
  return(indexes)
}


save_image = function(results){
    save(results, file = "SimulacoesAmostragemLog.RData")
    return(0)
}


