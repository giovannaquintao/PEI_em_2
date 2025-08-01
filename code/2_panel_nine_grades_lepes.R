### CS DID rodando 17-18 mar 
library(tidyverse)
library(did) # Callaway & Sant'Anna
library(tictoc)


address<-"C:/Users/giova/OneDrive/raw_data/lepes/"


load(paste0(address,"2305_RODANDO_EV.RData"))

names(base_0917_E_DE)
df<-base_0917_E_DE%>% 
  mutate(has_ever_been_treated=ifelse(ano_implantacao_esc_3EM!=0,1,0)) %>%
  mutate(pre_treatment=ifelse(ano<ano_implantacao_esc_3EM,1,0)) %>% 
  mutate(race=case_when(cor%in%c(1,4)~"white",
                        cor%in%c(2,3)~"black",
                        cor==5~"indigenous",
                        cor==6~"non_declared")) %>% 
  mutate(woman=ifelse(gen==2,1,0)) %>% 
  rename(
         id_student_seduc=id_aluno,
         id_school_seduc=esc,
         year_treatment=ano_implantacao_esc_3EM,
         age=idade,
         #age_entry=idade_entrada,
         correct_age=idade_certa,
         #age_formation=idade_formacao,
         adm_region=Regional,
         year=ano) 

df<-df %>% 
  select(id_student_seduc,year,id_school_seduc,year_treatment,has_ever_been_treated,pre_treatment,age,
         correct_age,drop_out, race,woman,adm_region,starts_with("lp_"),starts_with("mat_")) %>% 
  mutate(race=ifelse(race%in%c("indigenous","non_declared"),"other",race))



write_csv(df, "database/nine_grade_sample.csv")
# 
# run_and_save_att <- function(df_sub,covariate_name,covariate_formula) {
#     # Rodar para o conjunto completo
#  
#     att <- att_gt(
#       yname = "drop_out",
#       tname = "year",
#       idname = "id_student_seduc",
#       gname = "year_treatment",
#       data = df_sub,
#       xformla = if (is.null(covariate_formula)) NULL else as.formula(covariate_formula),
#       est_method = "dr",
#       control_group = "nevertreated",
#       bstrap = TRUE,
#       biters = 1000,
#       clustervars = "id_school_seduc",
#       panel = FALSE
#     )
#     
#     save(att, file = paste0("results/atts_evasao_", covariate_name, ".RData"))
# 
#     }
#   
# 
# covs1 <- "~ lp_9EF + mat_9EF + age + race + woman"
# covs2 <- "~ age + race + woman"
# covs3 <- "~ lp_9EF + mat_9EF"
# covs4 <- NULL
# 
# run_and_save_att(df, "cov", covs1)
# run_and_save_att(df, "individual_cov", covs2)
# run_and_save_att(df, "grade_cov", covs3)
# run_and_save_att(df, "nocov", NULL)
# future_map(tasks, run_att)
