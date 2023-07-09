estratificada_rede_500 = function(df_alunos){
    
    real_log_port = df_alunos |>
     dplyr::pull(log_port) |>
     mean()
    
    set.seed(123)
    AES_estimates_unif_rede = AES_estimates(df_alunos, strat_name = 'rede', rep_size = 1000, sample_size = 500, sample_size_type = 'uniform')
    AES_estimates_prop_rede = AES_estimates(df_alunos, strat_name = 'rede', rep_size = 1000, sample_size = 500, sample_size_type = 'proportional')
    AES_estimates_neyman_rede = AES_estimates(df_alunos, strat_name = 'rede', rep_size = 1000, sample_size = 500, sample_size_type = 'neyman')

    AES_estimates_table_rede =  list(AES_estimates_unif_rede, AES_estimates_prop_rede, AES_estimates_neyman_rede) |>
      map(~aess_table(., real_log_port)) 

    return(AES_estimates_table_rede)
}

estratificada_escola_500 = function(df_alunos){
    
    real_log_port = df_alunos |> # nolint
     dplyr::pull(log_port) |>
     mean()
    
    set.seed(607)
    AES_estimates_unif_escola = AES_estimates(df_alunos, strat_name = 'escola', rep_size = 1000, sample_size = 500, sample_size_type = 'uniform')
    AES_estimates_prop_escola = AES_estimates(df_alunos, strat_name = 'escola', rep_size = 1000, sample_size = 500, sample_size_type = 'proportional')
    AES_estimates_neyman_escola = AES_estimates(df_alunos, strat_name = 'escola', rep_size = 1000, sample_size = 500, sample_size_type = 'neyman')

    AES_estimates_table_escola =  list(AES_estimates_unif_escola, AES_estimates_prop_escola, AES_estimates_neyman_escola) |>
      map(~aess_table(., real_log_port)) 

    return(AES_estimates_table_escola)
}

estratificada_rede_750 = function(df_alunos){
    
    real_log_port = df_alunos |>
     dplyr::pull(log_port) |>
     mean()
    
    set.seed(123)
    AES_estimates_unif_rede = AES_estimates(df_alunos, strat_name = 'rede', rep_size = 1000, sample_size = 750, sample_size_type = 'uniform')
    AES_estimates_prop_rede = AES_estimates(df_alunos, strat_name = 'rede', rep_size = 1000, sample_size = 750, sample_size_type = 'proportional')
    AES_estimates_neyman_rede = AES_estimates(df_alunos, strat_name = 'rede', rep_size = 1000, sample_size = 750, sample_size_type = 'neyman')

    AES_estimates_table_rede =  list(AES_estimates_unif_rede, AES_estimates_prop_rede, AES_estimates_neyman_rede) |>
      map(~aess_table(., real_log_port)) 

    return(AES_estimates_table_rede)
}

estratificada_escola_750 = function(df_alunos){
    
    real_log_port = df_alunos |> # nolint
     dplyr::pull(log_port) |>
     mean()
    
    set.seed(607)
    AES_estimates_unif_escola = AES_estimates(df_alunos, strat_name = 'escola', rep_size = 1000, sample_size = 750, sample_size_type = 'uniform')
    AES_estimates_prop_escola = AES_estimates(df_alunos, strat_name = 'escola', rep_size = 1000, sample_size = 750, sample_size_type = 'proportional')
    AES_estimates_neyman_escola = AES_estimates(df_alunos, strat_name = 'escola', rep_size = 1000, sample_size = 750, sample_size_type = 'neyman')

    AES_estimates_table_escola =  list(AES_estimates_unif_escola, AES_estimates_prop_escola, AES_estimates_neyman_escola) |>
      map(~aess_table(., real_log_port)) 

    return(AES_estimates_table_escola)
}

