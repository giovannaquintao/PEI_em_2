library(tidyverse)
library(purrr)

rm(list=ls())
gc()

k<-readRDS("C:/Users/giova/OneDrive/raw_data/lepes/todas_matriculas_2008-2021.rds")

table(k$matriculas2008$dp_adm, useNA = "always")

categorize_matriculas <- function(df) {
  df %>%
    mutate(category = case_when(
      str_detect(etapa, "EJA") ~ "EJA",
      str_detect(etapa, "PAI - PROGRAMA DE ALFABETIZACAO E INCLUSAO") ~ "EJA",
      str_detect(etapa, "EDUCACAO ESPECIAL|DEFICIENCIA") ~ "SPECIAL",
      str_detect(etapa, "ENSINO MEDIO") ~ "EM",
      str_detect(etapa, "ENSINO FUNDAMENTAL") ~ "EF",
      TRUE ~ "OTHER"
    ))
}

# aplica a função a todos os anos de 2008 a 2020
for (ano in 2008:2020) {
  nome_df <- paste0("matriculas", ano)
  k[[nome_df]] <- categorize_matriculas(k[[nome_df]])
}


#filter only ninth grade
df9 <- map_dfr(2008:2020,
               ~ k[[paste0("matriculas", .x)]]
               %>% filter(serie == 9&category=="EF"))


df9<-df9 %>% 
  mutate(ano_after=ano+1) %>% 
  mutate(ano_after_2=ano+2) %>% 
  mutate(state=str_detect(dp_adm, "ESTADUAL")) %>% 
  filter(state==TRUE)

df9<-df9 %>% 
  group_by(id_aluno) %>% 
  mutate(min_year=min(ano)) %>% 
  filter(ano==min_year)


target_students<-as.vector(df9$id_aluno)

dfa <- map_dfr(2009:2020,
                 ~ k[[paste0("matriculas", .x)]]
                 %>% filter(id_aluno%in%target_students))
  
  


dfa<-dfa %>% 
  select(id_aluno, dp_adm, ano, esc, serie, etapa, category) %>% 
  rename_with(~ paste0(., "_after"))

  dfa <- dfa %>% 
    arrange(id_aluno_after, ano_after,desc(serie_after)) %>% 
    group_by(id_aluno_after, ano_after) %>%
    mutate(max_serie = max(serie_after,na.rm=T),
           obs_number=row_number()) %>% 
    filter(serie_after==max_serie)
  
  dfa<-dfa %>% 
    filter(obs_number==1) %>% 
    ungroup() %>% 
    select(-obs_number)

dfa2<-dfa %>% 
  rename_with(~ paste0(., "_2"))
 
dfall<-df9 %>% 
  left_join(dfa,by=c("id_aluno"="id_aluno_after",
                     "ano_after"="ano_after"))%>% 
  left_join(dfa2,by=c("id_aluno"="id_aluno_after_2",
                     "ano_after_2"="ano_after_2"))

write_csv(dfall,"database/nine_grade_student.csv")
