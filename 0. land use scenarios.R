# Universidade Federal do Ceará
# Programa de Pós-Graduação em Engenharia de Transportes
# Nelson de O. Quesado Filho
# Fevereiro de 2025
rm(list = ls()); gc()

# preambulo -----------------------------------------------------------------------------------
library(tidyverse)
#library(tidylog)
library(readxl)
library(aopdata)
library(data.table)

library(sf)
sf::sf_use_s2(FALSE)

# data loading --------------------------------------------------------------------------------
aop <- aopdata::read_grid('Fortaleza') %>% 
  select(hex = id_hex) %>% 
  left_join(aopdata::read_landuse('Fortaleza', year = 2019) %>% 
              select(hex = id_hex, jobs.low = T002, jobs.med = T003, jobs.high = T004))

sousa2019 <- 
  read_sf('data/landuse/dados socioeconomicos') %>% 
  sf::st_make_valid() %>% 
  mutate(JOBS.15 = IND+ADM+COM+SER+EDU) %>% 
  select(subzonas, AR.15 = AR, MR.15 = MR, BR.15 = BR, JOBS.15) %>% 
  left_join(read_xlsx('data/landuse/Dados Coletados.xlsx', sheet = 'Dados 2000') %>% 
              mutate(JOBS.00 = IND+ADM+COM+SER+EDU) %>% 
              select(subzonas = SZ, AR.00 = AR, MR.00 = MR, BR.00 = BR, JOBS.00),
            by = 'subzonas') %>% 
  select(-geometry)

zones.dic <- 
  st_centroid(aop) %>% 
  select(hex) %>% 
  st_join(sousa2019 %>% 
            select(subzonas) %>% 
            st_transform(crs = 4326),
          join = st_intersects) %>% 
  tibble() %>% 
  na.omit() %>% 
  select(-geom)

outliers <- 
  fread('data/landuse/outlier.csv')$id

ferreira2023 <- 
  read_rds('data/data.micro.final2.RDS') %>%
  select(hex, Rend_pc, work.class = V0648, age = V6036, student = V0628, income.class = V6530) %>% 
  filter(age >= 18 & age <= 80) %>% 
  mutate(work.class = if_else(is.na(work.class), 'non.worker', (if_else(work.class %in% c(4, 5, 6, 7) , 'non.formal', 'formal'))),
         student = if_else(student %in% 1:2, 'student', 'non.student'),
         income.class = if_else(income.class <= 3, 'BR', if_else(income.class > 8, 'AR', 'MR')),
         id = paste0('ind', 1:nrow(.))) %>% 
  filter(!id %in% outliers) %>% 
  tibble() %>% 
  mutate_at(c(1, 3, 5, 6), as_factor)

rm(outliers)
  
# growth rates --------------------------------------------------------------------------------
growth.rate <- 
  sousa2019 %>% 
  mutate_at(6:9, function(x){if_else(x == 0, 1, x)}) %>% 
  mutate(AR = AR.15 / AR.00,
          MR = MR.15 / MR.00,
          BR = BR.15 / BR.00,
          JOBS = JOBS.15 / JOBS.00) %>% 
  tibble() %>% 
  select(1, 14, 13, 12, 11) %>% 
  mutate(JOBS = if_else(JOBS == 0, min(JOBS[JOBS != 0])/2, JOBS),
         AR = if_else(AR == 0, min(AR[AR != 0])/2, AR),
         MR = if_else(MR == 0, min(MR[MR != 0])/2, MR),
         BR = if_else(BR == 0, min(BR[BR != 0])/2, BR)) %>% 
  left_join(., zones.dic) %>% 
  select(6, 2:5, -subzonas) %>% 
  pivot_longer(2:5, names_to = 'income.class', values_to = 'growth.rate')

# population ----------------------------------------------------------------------------------
sample_params <- 
  ferreira2023 %>%
  group_by(hex, income.class) %>% 
  reframe(original = n()) %>% 
  left_join(growth.rate, by = join_by(hex, income.class)) %>%
  mutate(neutral = original,
         pessimist = neutral * growth.rate,
         optimist = neutral / growth.rate) %>%
  select(-growth.rate) %>% 
  group_by(income.class) %>% 
  mutate(optimist = optimist / sum(optimist, na.rm = T) * sum(pessimist, na.rm = T),
         neutral = neutral / sum(neutral, na.rm = T) * sum(pessimist, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_longer(4:6, values_to = 'n', names_to = 'scenario') %>% 
  na.omit %>% 
  mutate(n = round(n))

samplear <- function(i){
  h <- sample_params[i, ]$hex
  c <- sample_params[i, ]$income.class
  n <- sample_params[i, ]$n
  
  petit.universe <- 
    ferreira2023 %>% 
    filter(hex == h & income.class == c)
  
  new.petit.universe <- 
    petit.universe[sample(1:nrow(petit.universe), size = n, replace = T), ] %>% 
    mutate(scenario = sample_params[i, ]$scenario)
  
  return(new.petit.universe)
}

populations <- lapply(1:nrow(sample_params), samplear)
beepr::beep()

populations <- 
  populations %>%
  rbindlist

populations <- 
  populations %>% 
  bind_rows(ferreira2023 %>% 
              mutate(scenario = 'original'))
          
fwrite(populations, 'data/landuse/populations.csv')

populations <- fread('data/landuse/populations.csv')

# jobs ----------------------------------------------------------------------------------------
jobs <- 
  aop %>% 
  pivot_longer(2:4, values_to = 'jobs', names_to = 'income.class') %>% 
  group_by(hex, income.class) %>% 
  reframe(original = sum(jobs, na.rm = T)) %>% 
  left_join(growth.rate %>% filter(income.class == 'JOBS') %>% select(-income.class)) %>% 
  mutate(pessimist = round(original * growth.rate),
         optimist = round(original / growth.rate)) %>% 
  select(- growth.rate) %>% 
  group_by(income.class) %>% 
  mutate(neutral = original / sum(original, na.rm = T) * sum(pessimist, na.rm = T),
         optimist = optimist / sum(optimist, na.rm = T) * sum(pessimist, na.rm = T)) %>% 
  pivot_longer(3:6, names_to = 'scenario', values_to = 'jobs') %>% 
  mutate(jobs = round(jobs),
         income.class = factor(income.class, levels = c('jobs.high', 'jobs.med', 'jobs.low')),
         scenario = factor(scenario, levels = c('original', 'optimist', 'neutral', 'pessimist')))

write_csv(jobs, 'data/landuse/jobs.csv')

jobs <- fread('data/landuse/jobs.csv')
# dicionario ----------------------------------------------------------------------------------
# AREA - Tamanho da zona em m²
# 
# IND - Empregos do Tipo Industrial - 2015
# ADM - Empregos do Tipo Administração Pública - 2015
# COM - Empregos do Tipo Comercial - 2015
# SER - Empregos do Tipo Prestação de Serviços - 2015
# EDU - Empregos do Tipo Educacional - 2015
# 
# CAT - Domicílios de Baixa Renda sem Modos Motorizados - 2015 
# BR - Domicílios de Baixa Renda com Modos Motorizados - 2015
# MR - Domicílios de Média Renda - 2015
# AR - Domicílios de Alta Renda - 2015
# 
# Baixa Renda - Até 3 S.M. de Renda Domiciliar
# Média Renda - Entre 3 e 8 S.M. de Renda Domiciliar
# Alta Renda - Acima de 8 S.M. de Renda Domiciliar

# V0628 – Frequenta escola ou creche
# Classificação da Informação:
# 1 – Sim, pública
# 2 – Sim, particular
# 3 – Não, já frequentou
# 4 – Não, nunca frequentou
# 
# V0661 commuting
# 1 - sim
# 2 - nao
# maps ----------------------------------------------------------------------------------------
geo.populations <- 
  populations %>% 
  group_by(hex, scenario, income.class) %>% 
  reframe(Rend_pc = mean(Rend_pc, na.rm = T),
          n = n()) %>% 
  left_join(aop %>% select(hex)) %>% 
  mutate(scenario = factor(scenario, levels = c('original', 'optimist', 'neutral', 'pessimist'))) %>% 
  st_as_sf()

geo.populations %>% 
  filter(scenario %in% c('optimist', 'pessimist')) %>% 
  group_by(scenario, income.class) %>% 
  mutate(n = n / sum(n, na.rm = T),
        Rend_pc = Rend_pc / sum(Rend_pc, na.rm = T),
        income.class = income.class %>% str_replace('BR', 'Low income') %>% str_replace('MR', 'Medium income') %>% str_replace('AR', 'High income') %>% 
          factor(., levels = c('High income', 'Medium income', 'Low income')),
        scenario = scenario %>% str_replace('optimist', 'As expected\n(With LU Policy)') %>% str_replace('pessimist', 'Problem evolution\n(Without LU Policy)')) %>% 
  na.omit() %>% 
  ggplot() +
  geom_sf(data = aop, fill = 'black', color = NA) +
  geom_sf(aes(fill = log10(n)), color = NA) +
  #geom_sf(aes(fill = n), color = NA) +
  viridis::scale_fill_viridis(option = 'H', breaks = c(-2, -6.25), labels = c('More\npeople', 'Less\npeople')) +
  theme_linedraw() +
  theme(legend.title = element_blank()) +
  facet_grid(income.class~scenario)
ggsave('figs/landuse.individuals.png', dpi = 320, width = 6, height = 6)

jobs %>% 
  filter(scenario %in% c('optimist', 'pessimist')) %>% 
  left_join(select(aop, hex)) %>% 
  mutate(income.class = income.class %>% str_remove('jobs.') %>% paste0(., ' complexity jobs') %>% 
           factor(levels = c("high complexity jobs", "med complexity jobs", "low complexity jobs")),
                  scenario = scenario %>% str_replace('optimist', 'As expected\n(With LU Policy)') %>% str_replace('pessimist', 'Problem evolution\n(Without LU Policy)'),
         jobs = if_else(jobs == 0, NA, jobs)) %>% 
  na.omit() %>% 
  st_as_sf() %>% 
  ggplot +
  geom_sf(data = aop, color = NA, fill = 'black') +
  #geom_sf(aes(fill = jobs), color = NA) +
  geom_sf(aes(fill = log10(jobs)), color = NA) +
  viridis::scale_fill_viridis(option = 'H', breaks = c(4, 1), labels = c("More\njobs", "Less\njobs")) +
  theme_linedraw() +
  theme(legend.title = element_blank()) +
  facet_grid(income.class~scenario)
ggsave('figs/landuse.jobs.png', dpi = 320, width = 6, height = 6)
