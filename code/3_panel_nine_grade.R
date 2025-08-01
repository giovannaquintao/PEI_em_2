library(tidyverse)
library(purrr)

rm(list=ls())
gc()


dfall<-read_csv("data/clean/nine_grade_student.csv")

#################### merge saresp data ############################

k<-readRDS("C:/Users/giova/OneDrive/raw_data/lepes/saresp_todos_2008-2022.rds")


dfall<-dfall %>% 
  group_by(id_aluno,ano) %>% 
  mutate(n=n()) %>% 
  filter(n==1)

k<-k%>% 
  group_by(id_aluno,ano) %>% 
  mutate(n=n()) %>% 
  filter(n==1)


dfall2<-dfall %>% 
  left_join(k %>% select(id_aluno,ano,profic_lp,profic_mat,ausencia),
            by=c("id_aluno","ano"))


dfall2<-dfall2 %>% 
  rename(id_school_inep=esc,
         id_school_inep_after=esc_after)

#################### merge school data ############################
schools<-read_csv("data/clean/schools_sp.csv")


dfall3 <- dfall2 %>%
  left_join(schools, by = c("id_school_inep"))


dfall3<-dfall3 %>% 
  select(id_aluno,ano,
         gen,cor,dt_nasc,mun_res,mun_nasc,profic_lp,profic_mat,ausencia,
         id_school_inep,id_classe,id_turma,etapa,serie,type_school,urban,category,starts_with("pei"),starts_with("sc_"),
         id_school_inep_after,serie_after,category_after)
names(df)

dfall4 <- dfall3 %>%
  left_join(schools, by = c( "id_school_inep_after"="id_school_inep"),suffix = c("", "_after"))  

#################### depedent variables ############################

names(dfall4)

dfall4<-dfall4 %>% 
  mutate(
    dropout = ifelse(is.na(id_school_inep_after_2), 1, 0),
    migrated_esc = ifelse(id_school_inep != id_school_inep_after, 1, 0),
    eja = ifelse(category_after == "EJA", 1, 0)
  )


#################### final clean ############################

dfall4 <- dfall4 %>%
  mutate(
    menina = ifelse(gen == 2, 1, 0),
    raca_nd = ifelse(cor == 6, 1, 0),
    negros_pardos = ifelse(!cor %in% c(2, 3), 1, 0),
    brancos = ifelse(cor %in% c(1, 4), 1, 0)
  )

dfall4<-dfall4 %>% 
  mutate(idade_entrada=ano-as.numeric(substr(dt_nasc,1,4)))

dfall4<-dfall4 %>% 
  rename(lp_9EF=profic_lp,
         mat_9EF= profic_mat)

write_csv(dfall4,"data/clean/panel_nine_graders.csv")

# fazer variaveis dependentes :

#dropout
#migrou de escola 
#EJA

#montar base de dados Callaway Santanna