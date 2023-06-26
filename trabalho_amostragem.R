library(tidyverse)
library(survey)
library(sampling)

df_alunos = read.table('https://raw.githubusercontent.com/Gustavo039/survey_articles/main/Alunos.txt', header = T)

df_alunos |> glimpse()

## Sampling  methods

set.seed(123)

## Amostragem por conglomerado 1 estágio (Metodo Iterativo)

media_por_escola = df_alunos |>
  group_by(escola) |>
  summarise(alunos = n()) |>
  pull(alunos) |>
  mean()

media_por_turma = df_alunos |>
  group_by(turma) |>
  summarise(alunos = n()) |>
  pull(alunos) |>
  mean()

df_alunos |>
  group_by(escola) |>
  summarise(turmas = length(unique(turma))) |>
  pull(turmas)|>
  summary()

select_from_n_cluster = function(n, var, ppt = F, tam = NULL){
    
    n_vars = length(var)
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


set.seed(123)
IACS =
  list("escola", "turma", c("escola", "turma"), c("rede", "escola", "turma")) |>
  map(~select_from_n_cluster(n = 750, .)) 

IACS |>
  map(~.$n)

IACS_ppt =
  mapply(
    function(var, tam){ select_from_n_cluster(n = 750, var = var, ppt = T, tam = tam) },
    var = c("escola", "escola"),
    tam =  c("turma", "alunos"),
    SIMPLIFY = F
  ) |>
  setNames(c("turma", "alunos"))

IACS_ppt |>
  map(~.$n)


## Amostragem por conglomerado 1 estágio (Metodo Via Grid de valores)
df_alunos |> group_by(escola) |> reframe(nmax = n_distinct(aluno), escola_id = as.character(escola)) |>
  ggplot2::ggplot(ggplot2::aes(escola_id, nmax)) +
  ggplot2::geom_point()




#### 1 estagio por escola 
doParallel::registerDoParallel()
replicate(500, 
  sampling::cluster(
    df_alunos,
    clustername = 'escola' ,
    size = 24,
    method = c("srswor")) |> 
  n_distinct('ID_unit') 
) |>
  hist()

#### 1 estagio por turma 
replicate(1000, 
          sampling::cluster(
            df_alunos,
            clustername = 'turma' ,
            size = 47,
            method = c("srswor")) |> 
            n_distinct('ID_unit') 
) |>
  hist()



#### 2 Estagios por escola e turma

sampling::mstage(
  df_alunos, 
  stage = c('cluster', 'cluster'),
  varnames = c('escola', 'turma'),
  size = list(1,1),
  method = c("srswor", "srswor")
)


doParallel::stopImplicitCluster()

##########################################################################

cluster_1_stage_schools = survey::svydesign(id = ~escola, 
                                            data = df_alunos, 
                                            fpc = ~fpc )

cluster_1_stage_classes = survey::svydesign(id = ~turma, 
                                            data = df_alunos, 
                                            fpc = ~fpc )

cluster_2_stage_schools_classes = survey::svydesign(id = ~escola + turma, 
                                                    data = df_alunos, 
                                                    fpc = ~fpc )








