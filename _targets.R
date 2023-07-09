library(targets)

files = paste0("R/", list.files("R/"))
lapply(files, source)

tar_option_set(packages = c("dplyr", "survey", "sampling", "tidyr", "purrr"))

list(
  tar_target(data, read_file()),
  tar_target(AES_estimates_table_rede_500, estratificada_rede_500(data)),
  tar_target(AES_estimates_table_escola_500, estratificada_escola_500(data)),

  tar_target(AES_estimates_table_rede_750, estratificada_rede_750(data)),
  tar_target(AES_estimates_table_escola_750, estratificada_escola_750(data)),

  tar_target(IACS_500, conglomerada_500(data)),
  tar_target(IACS_750, conglomerada_750(data)),
  tar_target(IACS_ppt_500, conglomerada_ppt_500(data)),
  tar_target(IACS_ppt_750, conglomerada_ppt_750(data)),

  tar_target(
    status, 
    save_image(
        list(
            "AES_estimates_table_rede_500" = AES_estimates_table_rede_500,
            "AES_estimates_table_escola_500" = AES_estimates_table_escola_500, 
            "AES_estimates_table_rede_750" = AES_estimates_table_rede_750,
            "AES_estimates_table_escola_750" = AES_estimates_table_escola_750, 

            "IACS_500" = IACS_500,
            "IACS_750" = IACS_750, 
            "IACS_ppt_500" = IACS_ppt_500, 
            "IACS_ppt_750" = IACS_ppt_750
            )), 
    cue = tar_cue("always"))
)


