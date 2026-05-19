# Universidade Federal do Ceará
# Programa de Pós-Graduação em Engenharia de Transportes
# Nelson de O. Quesado Filho
# Fevereiro de 2025
rm(list = ls()); gc()

# preambulo ----
#remotes::install_github('OPATP/GTFSwizard@main', force = T, update = 'always')
library(tidyverse)
library(tidylog)
library(GTFSwizard)
library(GA)
library(aopdata)
library(sf)

options(java.parameters = "-Xmx6G")
library(r5r)

rstudioapi::getActiveDocumentContext()$path |> 
  dirname() |> 
  setwd(); setwd('..')

# gtfs filter regular bus baseline ----
gtfs <-
  GTFSwizard::read_gtfs('data/gtfs/1.a bus.baseline.full.zip') %>%
  GTFSwizard::filter_date(date = '2021-12-13') %>% 
  GTFSwizard::filter_time(from = '06:20:00', to = '09:40:00')

pseudo.trips <- # pseudo trips are trips with only one stop_time
  gtfs$stop_times %>%
  dplyr::group_by(trip_id) %>%
  dplyr::reframe(n = n()) %>%
  dplyr::filter(n == 1) %>%
  dplyr::pull(trip_id)

bus.baseline <-
  gtfs %>% 
  GTFSwizard::filter_trip(trip = pseudo.trips, keep = FALSE) %>% 
  GTFSwizard::get_shapes() 

bus.baseline$trips <- 
  bus.baseline$trips %>% 
  mutate(direction_id = if_else(str_sub(trip_id, -1) == 'I', 1, 0))

bus.baseline %>% plot
bus.baseline %>% summary
bus.baseline %>% GTFSwizard::write_gtfs('data/gtfs/1.b bus.baseline.zip')

rm(gtfs, pseudo.trips)

# gtfs filter metro baseline ----
gtfs <-
  GTFSwizard::read_gtfs('data/gtfs/3.a metro.baseline.full.zip') %>%
  GTFSwizard::filter_date(date = '2021-12-13') %>% 
  GTFSwizard::filter_time(from = '06:20:00', to = '09:40:00')

pseudo.trips <- 
  gtfs$stop_times %>%
  dplyr::group_by(trip_id) %>%
  dplyr::reframe(n = n()) %>%
  dplyr::filter(n == 1) %>%
  dplyr::pull(trip_id)

metro.baseline <- 
  gtfs %>% 
  GTFSwizard::filter_trip(trip = pseudo.trips, keep = FALSE) %>% 
  GTFSwizard::get_shapes() 

metro.baseline %>% summary()

metro.baseline %>%
  write_gtfs('data/gtfs/3.b metro.baseline.zip')

rm(gtfs, pseudo.trips)

# gtfs filter metro future ----
gtfs <-
  GTFSwizard::read_gtfs('data/gtfs/4.a metro.future.full.zip') %>%
  GTFSwizard::filter_date(date = '2021-12-13') %>% 
  GTFSwizard::filter_time(from = '06:20:00', to = '09:40:00')

pseudo.trips <- 
  gtfs$stop_times %>%
  dplyr::group_by(trip_id) %>%
  dplyr::reframe(n = n()) %>%
  dplyr::filter(n == 1) %>%
  dplyr::pull(trip_id)

metro.future <- 
  gtfs %>% 
  GTFSwizard::filter_trip(trip = pseudo.trips, keep = FALSE) %>% 
  GTFSwizard::get_shapes() 

metro.future %>% summary()

metro.future %>%
  write_gtfs('data/gtfs/4.b metro.future.zip')

rm(gtfs, pseudo.trips)
