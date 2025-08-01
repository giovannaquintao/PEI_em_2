### CS DID rodando 17-18 mar 
library(tidyverse)
library(did) # Callaway & Sant'Anna
library(tictoc)
library(labelled)
library(kableExtra)


rm(list=ls())

#baixar base do lepes

address<-"C:/Users/giova/OneDrive/raw_data/lepes/"

#address<-"database"
load(paste0(address,"0507_BASE_CSdid.RData"))

df<-base_CallawaySantAnna_X


school <- df %>%
  distinct(ESC3EM, ano_implantacao_esc_3EM) %>%
  mutate(total = nrow(.)) %>%
  filter(ano_implantacao_esc_3EM != 0) %>%
  group_by(ano_implantacao_esc_3EM) %>%
  summarise(n = n(), total = first(total), .groups = "drop") %>%
  arrange(ano_implantacao_esc_3EM) %>%
  mutate(
    cumulative_n = cumsum(n),
    cumulative_pct = cumulative_n / total * 100,
    label = paste0(cumulative_n, "\n(", round(cumulative_pct, 1), "%)")
  )

ggplot(school, aes(x = ano_implantacao_esc_3EM, y = cumulative_n)) +
  geom_col(fill = "#1b9e77") +
  geom_text(aes(label = label), vjust = -0.1, color = "black", size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
  labs(x = "Year", y = NULL) +
  theme_minimal() +
  scale_x_continuous(breaks = school$ano_implantacao_esc_3EM) +
  theme(
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

ggsave("results/number_schools.png", width = 8, height = 5, dpi = 300)



############### other analysis @@@@@@@@@@@@@@@@@

df<-read_csv("data/clean/schools_sp.csv")

names(df)

school <- df %>%
  filter(sc_em==1&type_school=="state") 


school<-school %>% 
  mutate(total = nrow(.)) %>%
  filter(is.na(pei_em)==F) %>%
  group_by(pei_em) %>%
  summarise(n = n(), total = first(total), .groups = "drop") %>%
  arrange(pei_em) %>% 
  mutate(
    cumulative_n = cumsum(n),
    cumulative_pct = cumulative_n / total * 100,
    label = paste0(cumulative_n, "\n(", round(cumulative_pct, 1), "%)")
  )

ggplot(school, aes(x = pei_em, y = cumulative_n)) +
  geom_col(fill = "#1b9e77") +
  geom_text(aes(label = label), vjust = -0.1, color = "black", size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
  labs(x = "Year", y = NULL) +
  labs(y = "Number of Schools", y = NULL) +
  theme_minimal() +
  scale_x_continuous(breaks = school$pei_em) +
  theme(
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

ggsave("results/number_schools_alternative.png", width = 8, height = 5, dpi = 300)



############################ Descriptive 12 grade sample ########################
library(tidyverse)
library(kableExtra)

df <- read_csv("data/clean/12_grade_sample.csv")

# Define variable labels
var_labels <- c(
  "age_entry" = "Age at Entry",
  "age_formation" = "Age at Completion",
  "woman" = "Female",
  "race_black_or_brown" = "Race: Black",
  "lp_9EF_z_baseline" = "Language Test Score in 9th Grade",
  "lp_3EM_z_baseline" = "Language Test Score in 12th Grade",
  "mat_9EF_z_baseline" = "Math Test Score in 9th Grade",
  "mat_3EM_z_baseline" = "Math Test Score in 12th Grade",
  "Nr_Students" = "Number of Students"
)

# Create race dummy
df <- df %>%
  mutate(race_black_or_brown = ifelse(race == "black", 1, 0))

vars <- setdiff(names(var_labels), "Nr_Students")

summary_stats <- function(data) {
  data %>%
    summarise(across(all_of(vars),
                     list(mean = ~mean(.x, na.rm = TRUE),
                          sd = ~sd(.x, na.rm = TRUE)),
                     .names = "{.col}_{.fn}")) %>%
    pivot_longer(everything()) %>%
    separate(name, into = c("variable", "stat"), sep = "_(?=[^_]+$)") %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    mutate(stat = paste0(round(mean, 3), " (", round(sd, 3), ")")) %>%
    select(variable, stat)
}

treated_post <- df %>%
  filter(has_ever_been_treated == 1, pre_treatment == 0) %>%
  summary_stats() %>%
  rename(`Treated_post` = stat) %>%
  bind_rows(tibble(variable = "Nr_Students",
                   Treated_post = as.character(nrow(df %>% filter(has_ever_been_treated == 1, pre_treatment == 0)))))

treated_pre <- df %>%
  filter(has_ever_been_treated == 1, pre_treatment == 1) %>%
  summary_stats() %>%
  rename(`Treated_pre` = stat) %>%
  bind_rows(tibble(variable = "Nr_Students",
                   Treated_pre = as.character(nrow(df %>% filter(has_ever_been_treated == 1, pre_treatment == 1)))))

control <- df %>%
  filter(has_ever_been_treated == 0) %>%
  summary_stats() %>%
  rename(`Control` = stat) %>%
  bind_rows(tibble(variable = "Nr_Students",
                   Control = as.character(nrow(df %>% filter(has_ever_been_treated == 0)))))

# Function to assign stars
add_stars <- function(p) {
  case_when(
    p <= 0.01 ~ "***",
    p <= 0.05 ~ "**",
    p <= 0.10 ~ "*",
    p <= 0.15 ~ ".",
    TRUE ~ ""
  )
}

# Compute test statistics and stars
test_results <- tibble(variable = vars) %>%
  rowwise() %>%
  mutate(
    test1 = list(tryCatch(
      t.test(
             df[[variable]][df$has_ever_been_treated == 0],
      df[[variable]][df$has_ever_been_treated == 1 & df$pre_treatment == 0]),
      error = function(e) NULL
    )),
    test2 = list(tryCatch(
      t.test(df[[variable]][df$has_ever_been_treated == 1 & df$pre_treatment == 1],
             df[[variable]][df$has_ever_been_treated == 1 & df$pre_treatment == 0]),
      error = function(e) NULL
    )),
    t1 = if (!is.null(test1)) round(diff(test1$estimate), 3)else NA,
    p1 = if (!is.null(test1)) test1$p.value else NA,
    t2 = if (!is.null(test2)) round(diff(test2$estimate), 3)else NA,
    p2 = if (!is.null(test2)) test2$p.value else NA,
    `Control vs Treated post` = ifelse(!is.na(t1), paste0(t1, add_stars(p1)), NA),
    `Treated pre vs Treated post` = ifelse(!is.na(t2), paste0(t2, add_stars(p2)), NA)
  ) %>%
  ungroup() %>%
  select(variable, `Control vs Treated post`, `Treated pre vs Treated post`)


summary_table <- control %>%
  full_join(treated_pre, by = "variable") %>%
  full_join(treated_post, by = "variable") %>%
  full_join(test_results, by = "variable") %>%
  mutate(Variable = var_labels[variable]) %>%
  select(Variable, Control, Treated_pre, Treated_post,
         `Control vs Treated post`, `Treated pre vs Treated post`)

colnames(summary_table) <- c(
  "Variable",
  "Control",
  "Treated Pre",
  "Treated Post",
  "Treated Post – Control",
  "Treated Post – Treated Pre"
)



summary_table %>%
  kable(format = "latex",
        booktabs = TRUE,
        escape = FALSE)  %>%
  kable_styling(latex_options = c("hold_position"), table.envir = "")
