library(tidyverse)
library(basedosdados)

rm(list=ls())
gc()
################### PEI cleaning #######################

df<-readxl::read_excel("C:/Users/giova/OneDrive/raw_data/lepes/2.etapas_pei_sp.xlsx")

df<-df %>% 
  select(-`ADESÃO PEI`)

names(df)
dfl<-df %>% 
 pivot_longer(., cols = -c("CIE","INEP","NOME_ESC"),
               names_to = "ano", 
               values_to = "pei") 

table(dfl$pei, useNA = "always")
table(dfl$ano, useNA = "always")

dfl2<-dfl %>% 
  filter(is.na(pei) == F) %>%
  select(-NOME_ESC) %>% 
  group_by(CIE, INEP,pei) %>%
  summarise(ano=min(ano,na.rm=T))

dfl2<-dfl2 %>%
  group_by(CIE, INEP) %>%
  mutate(n=n())

dfl2<-dfl2 %>%
  pivot_wider(., 
               names_from = pei, 
               values_from = ano) 

names(dfl2)
dfl2 <- dfl2 %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    pei_af = min(c_across(c("AI AF", "AI AF EM", "AF", "AF EM")), na.rm = TRUE),
    pei_em = min(c_across(c("AF EM", "AI AF EM", "EM")), na.rm = TRUE),
    pei_ai = min(c_across(c("AI", "AI AF", "AI AF EM")), na.rm = TRUE)
  ) %>%
  ungroup()

pei<-dfl2 %>% 
  select(CIE, INEP, pei_af, pei_em, pei_ai) 

rm(df,dfl,dfl2)
gc()

################### School Census #######################


set_billing_id("humancapitalindex")
# Consulta SQL para obter correspondência entre id_escola e id_escola_sp
query <- "
SELECT DISTINCT
  id_escola,
  id_escola_sp
FROM `basedosdados.br_sp_seduc_fluxo_escolar.escola`
"

# Executa a consulta
df_corresp <- read_sql(query)
df_corresp <-df_corresp %>%
rename(id_school_inep=id_escola) %>% 
  rename(id_school_seduc=id_escola_sp)
# Consulta SQL para obter correspondência entre id_escola e id_escola_sp
query <- "
SELECT  *
FROM `basedosdados.br_inep_censo_escolar.escola`
WHERE sigla_uf = 'SP' AND ano <= 2019
"

school <- read_sql(query)

names(school)

school2<-school %>% 
  select(ano,id_municipio, id_escola,rede,tipo_localizacao,etapa_ensino_fundamental_anos_iniciais,
         etapa_ensino_fundamental_anos_finais,etapa_ensino_medio,
         etapa_ensino_eja)

school2<-school2 %>% 
  rename(sc_ai=etapa_ensino_fundamental_anos_iniciais,
         sc_af=etapa_ensino_fundamental_anos_finais,
         sc_em=etapa_ensino_medio,
         sc_eja=etapa_ensino_eja)

school2 <- school2 %>%
  mutate(urban= ifelse(tipo_localizacao == 1, 1, 0)) %>%
  mutate(type_school = case_when(
    rede == 1 ~ "federal",
    rede == 2 ~ "state",
    rede == 3 ~ "municipal",
    rede == 4 ~ "private"
  )) %>% 
  rename(id_school_inep=id_escola) 

school3 <- school2 %>%
  arrange(ano, id_school_inep) %>%
  group_by(id_school_inep) %>%
  summarise_all(last)

school3<-school3 %>% 
  select(-ano)

school3 <- school3 %>%
  left_join(df_corresp, by = "id_school_inep") 

names(pei)
school4 <- school3 %>%
  mutate(id_school_inep=as.numeric(id_school_inep)) %>%
  left_join(pei, by = c("id_school_inep"= "INEP"),
            c("id_school_seduc"="CIE"))

names(school4)

school4<-school4 %>% 
  select(id_school_inep, id_school_seduc,id_municipio,urban,type_school,
        starts_with("pei_"), starts_with("sc_"))

school4<-school4 %>% 
  rename(id_mun_sch=id_municipio)

write_csv(school4, file = "data/clean/schools_sp.csv")
