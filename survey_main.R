df_alunos = read.table('Alunos.txt', header = T) |>
  dplyr::mutate(fpc = (max(aluno) - dplyr::n_distinct(escola)) /(max(aluno)-1))

## Sampling  methods

cluster_1_stage_schools = survey::svydesign(id = ~escola, 
                                            data = df_alunos, 
                                            fpc = ~fpc )

cluster_1_stage_classes = survey::svydesign(id = ~turma, 
                                            data = df_alunos, 
                                            fpc = ~fpc )

cluster_2_stage_schools_classes = survey::svydesign(id = ~escola + turma, 
                                            data = df_alunos, 
                                            fpc = ~fpc )


## Response variable transformation


