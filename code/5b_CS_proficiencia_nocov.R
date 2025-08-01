library(tidyverse)
library(did)
library(furrr)
library(future)
library(tictoc)
rm(list=ls())
gc()
# Setup: cada combinação roda num núcleo
plan(multisession, workers = 4)
plan(multicore, workers = 4)  # ou plan(multicore) no Linux/Mac

# Carrega e prepara base
all <- read_csv("database/12_grade_sample.csv") %>%
  mutate(
    quintile_lp_9EF = ntile(lp_9EF, 5),
    quintile_mat_9EF = ntile(mat_9EF, 5),
    race = ifelse(race %in% c("indigenous", "non_declared"), "other", race)
  )


dir.create("results", showWarnings = FALSE)

# Lista de combinações outcome + covariada
tasks <- list(
  list(outcome = "lp", cov = TRUE),
  list(outcome = "lp", cov = FALSE),
  list(outcome = "mat", cov = TRUE),
  list(outcome = "mat", cov = FALSE)
)

# Função para rodar cada tarefa
run_att <- function(task) {
  outcome <- task$outcome
  use_cov <- task$cov
  
  yname_var <- paste0(outcome, "_3EM_z_baseline")
  baseline_var <- paste0(outcome, "_9EF_z_baseline")
  
  if (use_cov) {
    xform <- as.formula(paste0("~", baseline_var))
    suffix <- "_only_9cov"
  } else {
    xform <- NULL
    suffix <- "_nocov"
  }
  
  cat("Running:", outcome, suffix, "\n")
  
  att <- att_gt(
    yname = yname_var,
    tname = "year",
    idname = "id_student_seduc",
    gname = "year_treatment",
    data = all,
    xformla = xform,
    est_method = "dr",
    control_group = "nevertreated",
    bstrap = TRUE,
    biters = 1000,
    clustervars = "id_school_seduc",
    panel = FALSE
  )
  
  save(att, file = paste0("results/atts_", outcome, suffix, ".RData"))
  rm(att)
  gc()
  
  return(paste("Finished", outcome, suffix))
}

# Rodar todas as combinações em paralelo
tic("Rodando 4 modelos (2 outcomes x 2 specs)")
future_map(tasks, run_att, .options = furrr_options(seed = TRUE))
toc()






