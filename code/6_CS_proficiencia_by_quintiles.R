### CS DID rodando 17-18 mar 
library(tidyverse)
library(did) # Callaway & Sant'Anna
library(tictoc)

rm(list=ls())
gc()
#baixar base do lepes



all <- read_csv("data/clean/12_grade_sample.csv")

all<- all%>%
  mutate(
    quintile_lp_9EF = ntile(lp_9EF, 5),
    quintile_mat_9EF = ntile(mat_9EF, 5)
  ) %>% 
  mutate(race=ifelse(race%in%c("indigenous","non_declared"),"other",race))


################## all ###################

outcomes <- c("lp", "mat")

for (outcome in outcomes) {
  for (q in 1:5) {
    
    # Nome das variáveis conforme o desfecho
    yname_var <- paste0(outcome, "_3EM_z_baseline")
    baseline_var <- paste0(outcome, "_9EF_z_baseline")
    quintile_var <- paste0("quintile_", outcome, "_9EF")
    
    # Filtra a amostra por quintil
    df <- all %>%
      filter(!!sym(quintile_var) == q)
    
    # Estima ATT com covariadas
    att_cov <- att_gt(
      yname = yname_var,
      tname = "year",
      idname = "id_student_seduc",
      gname = "year_treatment",
      data = df,
      xformla = as.formula(paste0("~", baseline_var, " + race + woman + age_entry")),
      est_method = "dr",
      control_group = "nevertreated",
      bstrap = TRUE,
      biters = 1000,
      clustervars = "id_school_seduc",
      panel = FALSE
    )
    
    # Salva o resultado
    save(att_cov, file = paste0("results/atts_", outcome, "_quintile", q, "_cov.RData"))
    
    print(paste0("Saved atts_", outcome, "_quintile", q))
    # Limpa para economizar memória
    rm(att_cov)
    gc()
  }
}
