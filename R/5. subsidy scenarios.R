# Universidade Federal do Ceará
# Programa de Pós-Graduação em Engenharia de Transportes
# Nelson de O. Quesado Filho
# Julho de 2024
rm(list = ls()); gc()

# preambulo ----
library(GTFSwizard)
library(tidyverse)
library(aopdata)
library(GA)
library(sf)
library(data.table)
library(igraph)
library(TSP)

sf_use_s2(FALSE)

options(java.parameters = '-Xmx2G')
library(r5r)

theme_gtfswizard <-
  hrbrthemes::theme_ipsum(base_family = "Times New Roman",
                          axis_title_face = 'bold') +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size = 10, color = '#283c42'),
    axis.title.y = ggplot2::element_text(size = 10, color = '#283c42'),
    axis.text.x = ggplot2::element_text(size = 9),
    axis.text.y = ggplot2::element_text(size = 9),
    legend.text = ggplot2::element_text(size = 9))

# lu data ----
aop <- aopdata::read_grid('Fortaleza') %>% 
  select(hex = id_hex) 

populations <-
  fread('data/landuse/populations.csv') %>% 
  filter(scenario == 'optimist') %>% 
  mutate_at(c(1, 3, 5:8), as_factor)

hex.origin <-
  populations %>% 
  filter(income.class == 'BR') %>% 
  .$hex %>% 
  unique

origins <- 
  read_grid(city = 'Fortaleza') %>%
  filter(id_hex %in% hex.origin) %>% 
  select(id = id_hex) %>% 
  st_centroid()

jobs <-
  fread('data/landuse/jobs.csv') %>% 
  filter(jobs != 0 & scenario == 'optimist') %>% 
  rename(job.class = income.class) %>% 
  left_join(aop) %>% 
  st_as_sf()

hex.destination <- 
  jobs %>%
  .$hex %>% 
  unique

destinations <-
  read_grid(city = 'Fortaleza') %>%
  filter(id_hex %in% hex.destination) %>% 
  select(id = id_hex) %>% 
  st_centroid()

shp.bairro <-
  read_sf('data/bairros', crs = 4326) %>%
  select(geometry)

# scenarios it1 -----------------------------------------------------------------------------------
performance <- 
  lapply(list.files(path = 'data/performance', 'performance.csv', full.names = TRUE),
         data.table::fread) %>% 
  data.table::rbindlist()

regular.vkt.budget <- performance$bus.vkt[1]
regular.fleet.budget <- performance$bus.fleet[1]

regular.revenue <- 
  populations %>% 
  group_by(income.class, work.class, student) %>% 
  reframe(n = n()) %>% 
  mutate(policy = if_else(income.class == 'BR' & student == 'non.student', 'BR.policy',
                          ifelse(student == 'student', 'student.policy', 'no.policy'))) %>% 
  group_by(policy) %>% 
  reframe(n = sum(n)) %>% 
  mutate(fare = c(4.5, 4.5, 1.5)) %>%
  mutate(contr = n * fare) %>% 
  .$contr %>% 
  sum
  
regular.revenue.br <- 
  populations %>% 
  filter(income.class == 'BR' & student == 'non.student') %>% 
    nrow() * 4.5
  
subsidy.scenarios <- 
  populations %>% 
  filter(income.class == 'BR' & student == 'non.student') %>% 
  reframe(BR = n(),
          fare = c(seq(2, 4.25, .25)),
          new.revenue.br = (fare * BR),
          revenue.reduction = new.revenue.br - regular.revenue.br,
          perc.revenue.reduction = revenue.reduction/regular.revenue,
          subsidy = list(seq(.05, .35, .05))) %>% 
  unnest(cols = subsidy) %>% 
  arrange(subsidy) %>% 
  mutate(new.vkt = (subsidy + perc.revenue.reduction + 1) * regular.vkt.budget,
         new.fleet = (subsidy + perc.revenue.reduction + 1) * regular.fleet.budget)

subsidy.scenarios %>% 
  fwrite('data/subsidy.scenarios.csv')

optimist.subsidy <- 
  subsidy.scenarios %>% 
  filter(subsidy == .35) %>% 
  mutate(id = paste0( LETTERS[1:n()], '.', subsidy) %>% 
           str_remove('0.')) %>% 
  select(id, fare, new.vkt, new.fleet)

# scenarios it2 -----------------------------------------------------------------------------------
performance <- 
  lapply(list.files(path = 'data/performance', 'performance.csv', full.names = TRUE),
         data.table::fread) %>% 
  data.table::rbindlist()

regular.vkt.budget <- performance$bus.vkt[1]
regular.fleet.budget <- performance$bus.fleet[1]

regular.revenue <- 
  populations %>% 
  group_by(income.class, work.class, student) %>% 
  reframe(n = n()) %>% 
  mutate(policy = if_else(income.class == 'BR' & student == 'non.student', 'BR.policy',
                          ifelse(student == 'student', 'student.policy', 'no.policy'))) %>% 
  group_by(policy) %>% 
  reframe(n = sum(n)) %>% 
  mutate(fare = c(4.5, 4.5, 1.5)) %>%
  mutate(contr = n * fare) %>% 
  .$contr %>% 
  sum

regular.revenue.br <- 
  populations %>% 
  filter(income.class == 'BR' & student == 'non.student') %>% 
  nrow() * 4.5

subsidy.scenarios <- 
  populations %>% 
  filter(income.class == 'BR' & student == 'non.student') %>% 
  reframe(BR = n(),
          fare = seq(0, 2, .5),
          new.revenue.br = (fare * BR),
          revenue.reduction = new.revenue.br - regular.revenue.br,
          perc.revenue.reduction = revenue.reduction/regular.revenue,
          subsidy = .6) %>% 
  arrange(subsidy) %>% 
  mutate(new.vkt = (subsidy + perc.revenue.reduction + 1) * regular.vkt.budget,
         new.fleet = (subsidy + perc.revenue.reduction + 1) * regular.fleet.budget) %>% 
    arrange(-subsidy, fare)

subsidy.scenarios %>% 
  fwrite('data/subsidy.scenarios.it-2.csv')

optimist.subsidy <- 
  subsidy.scenarios %>% 
  filter(subsidy == .35) %>% 
  mutate(id = paste0( LETTERS[1:n()], '.', subsidy) %>% 
           str_remove('0.')) %>% 
  select(id, fare, new.vkt, new.fleet)

# scenarios it3 -----------------------------------------------------------------------------------
performance <- 
  lapply(list.files(path = 'data/performance', 'performance.csv', full.names = TRUE),
         data.table::fread) %>% 
  data.table::rbindlist()

regular.vkt.budget <- performance$bus.vkt[1]
regular.fleet.budget <- performance$bus.fleet[1]

regular.revenue <- 
  populations %>% 
  group_by(income.class, work.class, student) %>% 
  reframe(n = n()) %>% 
  mutate(policy = if_else(income.class == 'BR' & student == 'non.student', 'BR.policy',
                          ifelse(student == 'student', 'student.policy', 'no.policy'))) %>% 
  group_by(policy) %>% 
  reframe(n = sum(n)) %>% 
  mutate(fare = c(4.5, 4.5, 1.5)) %>%
  mutate(contr = n * fare) %>% 
  .$contr %>% 
  sum

regular.revenue.br <- 
  populations %>% 
  filter(income.class == 'BR' & student == 'non.student') %>% 
  nrow() * 4.5

subsidy.scenarios <- 
  populations %>% 
  filter(income.class == 'BR' & student == 'non.student') %>% 
  reframe(BR = n(),
          fare = 0,
          new.revenue.br = (fare * BR),
          revenue.reduction = new.revenue.br - regular.revenue.br,
          perc.revenue.reduction = revenue.reduction/regular.revenue,
          subsidy = seq(0, .5, .1)) %>% 
  arrange(subsidy) %>% 
  mutate(new.vkt = (subsidy + perc.revenue.reduction + 1) * regular.vkt.budget,
         new.fleet = (subsidy + perc.revenue.reduction + 1) * regular.fleet.budget) %>% 
  arrange(-subsidy, fare)

subsidy.scenarios %>% 
  fwrite('data/subsidy.scenarios.it-3.csv')

# scenarios it4 -----------------------------------------------------------------------------------
performance <- 
  lapply(list.files(path = 'data/performance', 'performance.csv', full.names = TRUE),
         data.table::fread) %>% 
  data.table::rbindlist()

regular.vkt.budget <- performance$bus.vkt[1]
regular.fleet.budget <- performance$bus.fleet[1]

regular.revenue <- 
  populations %>% 
  group_by(income.class, work.class, student) %>% 
  reframe(n = n()) %>% 
  mutate(policy = if_else(income.class == 'BR' & student == 'non.student', 'BR.policy',
                          ifelse(student == 'student', 'student.policy', 'no.policy'))) %>% 
  group_by(policy) %>% 
  reframe(n = sum(n)) %>% 
  mutate(fare = c(4.5, 4.5, 1.5)) %>%
  mutate(contr = n * fare) %>% 
  .$contr %>% 
  sum

regular.revenue.br <- 
  populations %>% 
  filter(income.class == 'BR' & student == 'non.student') %>% 
  nrow() * 4.5

subsidy.scenarios <- 
  populations %>% 
  filter(income.class == 'BR' & student == 'non.student') %>% 
  reframe(BR = n(),
          fare = 0,
          new.revenue.br = (fare * BR),
          revenue.reduction = new.revenue.br - regular.revenue.br,
          perc.revenue.reduction = revenue.reduction/regular.revenue,
          subsidy = .25) %>% 
  arrange(subsidy) %>% 
  mutate(new.vkt = (subsidy + perc.revenue.reduction + 1) * regular.vkt.budget,
         new.fleet = (subsidy + perc.revenue.reduction + 1) * regular.fleet.budget) %>% 
  arrange(-subsidy, fare)

subsidy.scenarios %>% 
  fwrite('data/subsidy.scenarios.it-4.csv')
