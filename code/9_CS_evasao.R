library(tidyverse)
library(did)
library(furrr)
library(future)
library(tictoc)
rm(list=ls())
gc()

df <- read_csv("database/nine_grade_sample.csv") 




# Definir plano paralelo
plan(multisession, workers = 4)  # ou plan(multicore) no Linux/Mac
plan(multicore, workers = 4)  # ou plan(multicore) no Linux/Mac

# Função fornecida
run_and_save_att <- function(df_sub, covariate_name, covariate_formula) {
  att <- att_gt(
    yname = "drop_out",
    tname = "year",
    idname = "id_student_seduc",
    gname = "year_treatment",
    data = df_sub,
    xformla = if (is.null(covariate_formula)) NULL else as.formula(covariate_formula),
    est_method = "dr",
    control_group = "nevertreated",
    bstrap = TRUE,
    biters = 1000,
    clustervars = "id_school_seduc",
    panel = FALSE
  )
  save(att, file = paste0("results/atts_evasao_", covariate_name, ".RData"))
}

# Lista de tarefas
task_list <- tibble(
  df_sub = list(df, df, df, df),
  covariate_name = c("cov", "individual_cov", "grade_cov", "nocov"),
  covariate_formula = list(
    "~ lp_9EF + mat_9EF + age + race + woman",
    "~ age + race + woman",
    "~ lp_9EF + mat_9EF",
    NULL
  )
)

# Executar em paralelo com future_pmap
future_pmap(task_list, run_and_save_att, .options = furrr_options(seed = TRUE))
