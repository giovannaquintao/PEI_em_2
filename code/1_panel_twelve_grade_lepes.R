### CS DID rodando 17-18 mar 
library(tidyverse)
library(did) # Callaway & Sant'Anna
library(tictoc)
library(labelled)
library(knitr)
library(kableExtra)


address<-"C:/Users/giova/OneDrive/raw_data/lepes/"


load(paste0(address,"0507_BASE_CSdid.RData"))

df<-base_CallawaySantAnna_X%>% 
  mutate(has_ever_been_treated=ifelse(ano_implantacao_esc_3EM!=0,1,0)) %>%
  mutate(pre_treatment=ifelse(ano_formacao<ano_implantacao_esc_3EM,1,0)) %>% 
  mutate(race=case_when(cor%in%c(1,4)~"white",
                        cor%in%c(2,3)~"black",
                        cor==5~"indigenous",
                        cor==6~"non_declared")) %>% 
  rename(woman=menina,
         id_student_seduc=id_aluno,
         id_school_seduc=ESC3EM,
         year_treatment=ano_implantacao_esc_3EM,
         age_entry=idade_entrada,
         age_formation=idade_formacao,
         adm_region=Regional,
         year=ano_formacao) 

df<-df %>% 
  select(id_student_seduc,year,id_school_seduc,year_treatment,has_ever_been_treated,pre_treatment,
         race,woman,adm_region,age_entry,age_formation,starts_with("lp_"),starts_with("mat_"))



# Add variable labels
var_label(df) <- list(
  id_student_seduc      = "Student ID (SEDUC)",
  year                  = "Year of high school completion",
  id_school_seduc       = "School ID (SEDUC)",
  year_treatment        = "Year of FTS adoption in school",
  has_ever_been_treated = "Treated school (1 = yes)",
  pre_treatment         = "Pre-treatment period (1 = yes)",
  race                  = "Student race",
  woman                 = "Female student (1 = yes)",
  adm_region            = "School administrative region",
  age_entry             = "Student's age at entry to high school",
  age_formation         = "Student's age at high school completion"
  # lp_* and mat_* variables can also be labeled similarly if needed
)

score_vars <- c("lp_9EF", "lp_3EM", "mat_9EF", "mat_3EM")


names(df)
df <- df %>%
  group_by(year) %>%
  mutate(across(all_of(score_vars), 
                ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
                .names = "{.col}_z")) %>% 
  ungroup()


# Define your score variables
score_vars <- c("lp_9EF", "lp_3EM", "mat_9EF", "mat_3EM")

names(df)
# Step 1: Calculate means and SDs in the baseline year (e.g., 2010)
baseline_stats <- df %>%
  filter(year <=2011) %>%
  summarise(across(all_of(score_vars),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

# Step 2: Extract into named vectors
means <- baseline_stats %>% select(ends_with("_mean")) %>% unlist()
sds   <- baseline_stats %>% select(ends_with("_sd")) %>% unlist()

# Clean names for indexing
names(means) <- gsub("_mean", "", names(means))
names(sds)   <- gsub("_sd", "", names(sds))

# Step 3: Standardize using baseline year stats
df <- df %>%
  mutate(across(all_of(score_vars),
                ~ (. - means[cur_column()]) / sds[cur_column()],
                .names = "{.col}_z_baseline"))

score_groups <- list(
  lp = c("lp_3EM"),
  mat = c("mat_3EM")
)

# 1. Calcular média e DP conjuntas para LP e MAT no período de referência (2009–2011)
baseline_df <- df %>% filter(year <= 2011)

# Calcular média e DP para LP
lp_vals <- baseline_df %>% select(all_of(score_groups$lp)) %>% unlist()
lp_mean <- mean(lp_vals, na.rm = TRUE)
lp_sd   <- sd(lp_vals, na.rm = TRUE)

# Calcular média e DP para MAT
mat_vals <- baseline_df %>% select(all_of(score_groups$mat)) %>% unlist()
mat_mean <- mean(mat_vals, na.rm = TRUE)
mat_sd   <- sd(mat_vals, na.rm = TRUE)

# 2. Padronizar no df original
df <- df %>%
  mutate(
    lp_9EF_z_joint  = (lp_9EF - lp_mean) / lp_sd,
    lp_3EM_z_joint  = (lp_3EM - lp_mean) / lp_sd,
    mat_9EF_z_joint = (mat_9EF - mat_mean) / mat_sd,
    mat_3EM_z_joint = (mat_3EM - mat_mean) / mat_sd
  )


write_csv(df, "data/clean/12_grade_sample.csv")
