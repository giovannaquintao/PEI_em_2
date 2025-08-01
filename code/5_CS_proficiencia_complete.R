### CS DID rodando 17-18 mar 
library(tidyverse)
library(did) # Callaway & Sant'Anna
library(tictoc)

rm(list=ls())
gc()
#baixar base do lepes



all <- read_csv("database/12_grade_sample.csv")


#all <- read_csv("data/clean/12_grade_sample.csv")
dir.create("results")

all<- all%>%
  mutate(
    quintile_lp_9EF = ntile(lp_9EF, 5),
    quintile_mat_9EF = ntile(mat_9EF, 5)
  ) %>% 
  mutate(race=ifelse(race%in%c("indigenous","non_declared"),"other",race))

names(all)
################## aggregate ###################
outcomes <- c("lp", "mat")
#outcomes <- c("mat")

for (outcome in outcomes) {
  
  # Define nomes das variáveis dinamicamente
  yname_var <- paste0(outcome, "_3EM_z_baseline")
  baseline_var <- paste0(outcome, "_9EF_z_baseline")
  
  # Estima ATT com covariadas
  att_cov <- att_gt(
    yname = yname_var,
    tname = "year",
    idname = "id_student_seduc",
    gname = "year_treatment",
    data = all,
    xformla = as.formula(paste0("~", baseline_var, " + race + woman + age_entry")),
    est_method = "dr",
    control_group = "nevertreated",
    bstrap = TRUE,
    biters = 1000,
    clustervars = "id_school_seduc",
    panel = FALSE
  )
  
  # Salva o resultado
  save(att_cov, file = paste0("results/atts_", outcome, "_cov.RData"))
  
  # Limpa memória
  rm(att_cov)
  gc()
}



