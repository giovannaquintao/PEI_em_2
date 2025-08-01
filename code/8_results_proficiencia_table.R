library(tidyverse)
library(did)
library(kableExtra)

  # Função auxiliar para carregar os ATT
load_aggte_dynamic <- function(subject, quintile = NULL) {
    if (is.null(quintile)) {
      file <- paste0("results/atts_", subject, "_cov", ".RData")
    } else {
      file <- paste0("results/atts_", subject, "_quintile", quintile, "_cov",  ".RData")
    }
  
    
    load(file)
    obj_name <- ls()[ls() != "file"][1]
    obj <- get(obj_name)
    rm(list = obj_name)
    
    ag <- aggte(obj, type = "simple")
    
    att <- round(ag$overall.att, 3)
    se <- round(ag$overall.se, 3)
    
    stars <- case_when(
      abs(att/se) > 2.58 ~ "***",  # ~1% significance
      abs(att/se) > 1.96 ~ "**",   # ~5% significance
      abs(att/se) > 1.64 ~ "*",    # ~10% significance
      TRUE ~ ""
    )
    
    return(paste0(att, stars, "\n(", se, ")"))
  }
  
  # Criar as linhas da tabela
  rows <- c("Overall", paste0(1:5, "st Quintil") %>%
              str_replace("^1st", "1st") %>%
              str_replace("^2st", "2nd") %>%
              str_replace("^3st", "3rd") %>%
              str_replace("^4st", "4th") %>%
              str_replace("^5st", "5th"))
  
  
  
  # Obter os valores para cada célula (sem loops explícitos)
  mat_vals <- c(
    load_aggte_dynamic("mat", NULL),
    load_aggte_dynamic("mat", 1),
    load_aggte_dynamic("mat", 2),
    load_aggte_dynamic("mat", 3),
    load_aggte_dynamic("mat", 4),
    load_aggte_dynamic("mat", 5)
  )
  
  lp_vals <- c(
    load_aggte_dynamic("lp", NULL),
    load_aggte_dynamic("lp", 1),
    load_aggte_dynamic("lp", 2),
    load_aggte_dynamic("lp", 3),
    load_aggte_dynamic("lp", 4),
    load_aggte_dynamic("lp", 5)
  )
  
  # Montar data.frame final
  table_att <- tibble(
    Group = rows,
    Mat = mat_vals,
    Lp = lp_vals
  )
  
  latex_table <- table_att %>%
    kable(
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      align = "lcc",
      col.names = c("Group", "Mathematics", "Language"),
      caption = NULL
    ) %>%
    kable_styling(
      latex_options = c("hold_position"), table.envir = ""
    ) %>%
    column_spec(1, bold = TRUE)
  latex_table
  # Save LaTeX code as plain .tex file (no table env)
  writeLines(latex_table, "results/att_table.tex")
  