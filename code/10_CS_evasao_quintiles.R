library(tidyverse)
library(did)
library(furrr)
library(future)
library(tictoc)

rm(list = ls())
gc()
set.seed(123)
# Carregar base e preparar variáveis
df <- read_csv("database/nine_grade_sample.csv") %>%
  mutate(
    quintile_lp_9EF = ntile(lp_9EF, 5),
    quintile_mat_9EF = ntile(mat_9EF, 5),
    race = ifelse(race %in% c("indigenous", "non_declared"), "other", race)
  )

# Definir plano paralelo e aumentar limite de tamanho dos objetos globais
options(future.globals.maxSize = 1 * 1024^3)  # 1 GB
plan(multisession, workers = 5)  # use multicore only on Unix-like systems
plan(multicore, workers = 4)  # ou plan(multicore) no Linux/Mac

# Função segura para rodar e salvar o ATT
run_and_save_att <- function(q) {
  df_sub <- df %>% filter(quintile_mat_9EF == q)
  
  if (nrow(df_sub) < 50) {
    message("Quintile ", q, " has too few observations. Skipping.")
    return(NULL)
  }
  
  tryCatch({
    att <- att_gt(
      yname = "drop_out",
      tname = "year",
      idname = "id_student_seduc",
      gname = "year_treatment",
      data = df_sub,
      xformla = ~ lp_9EF + mat_9EF + age + race + woman,
      est_method = "dr",
      control_group = "nevertreated",
      bstrap = TRUE,
      biters = 1000,
      clustervars = "id_school_seduc",
      panel = FALSE
    )
    
    save(att, file = paste0("results/atts_evasao_quintile", q, ".RData"))
    message("Saved: results/atts_evasao_quintile", q, ".RData")
  }, error = function(e) {
    message("Error in quintile ", q, ": ", e$message)
  })
}

# Executar em paralelo
future_map(1:5, run_and_save_att, .options = furrr_options(seed = TRUE))

