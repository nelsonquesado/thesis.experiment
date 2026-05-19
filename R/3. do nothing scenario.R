# Universidade Federal do Ceará
# Programa de Pós-Graduação em Engenharia de Transportes
# Nelson de O. Quesado Filho
# Julho de 2024
rm(list = ls()); gc()

# preambulo ----
library(tidyverse)
library(tidylog)
library(data.table)

library(GTFSwizard)

library(aopdata)

options(java.parameters = "-Xmx6G")
library(r5r)
library(sf)

theme_gtfswizard <-
  hrbrthemes::theme_ipsum(base_family = "Times New Roman",
                          axis_title_face = 'bold') +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size = 10, color = '#283c42'),
    axis.title.y = ggplot2::element_text(size = 10, color = '#283c42'),
    axis.text.x = ggplot2::element_text(size = 9),
    axis.text.y = ggplot2::element_text(size = 9),
    legend.text = ggplot2::element_text(size = 9))

# ts data ----
bus.gtfs.file <- 'data/gtfs/1.b bus.baseline.zip'
bus.gtfs <- read_gtfs(bus.gtfs.file)

metro.gtfs.file <- 'data/gtfs/3.b metro.baseline.zip'
metro.gtfs <- read_gtfs(metro.gtfs.file)

metro.gtfs$stops$stop_id <- paste0('M', metro.gtfs$stops$stop_id)
metro.gtfs$stop_times$stop_id <- paste0('M', metro.gtfs$stop_times$stop_id)

# lu data ----
aop <- aopdata::read_grid('Fortaleza') %>% 
  select(hex = id_hex) 

populations <-
  fread('data/landuse/populations.csv') %>% 
  filter(scenario == 'pessimist') %>% 
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
  filter(jobs != 0 & scenario == 'pessimist') %>% 
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

g1 <- 
  jobs %>%
  filter(job.class == 'jobs.low') %>% 
  ggplot() +
  geom_sf(data = aop, fill = 'black', color = NA) +
  geom_sf(aes(fill = jobs),
          color = NA) +
  theme_gtfswizard +
  viridis::scale_fill_viridis(option = 'H', labels = scales::label_number(big.mark = ",")) +
  labs(fill = 'Low education\njob poisitions', title = 'b) Travel attraction') +
  ggspatial::annotation_scale(location = "bl", text_cex = 0.8, bar_cols = c("#333333", 'white')) +
  ggspatial::annotation_north_arrow(location = "tr",
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c('white', '#333333'),
                                                                                      line_col = '#333333',
                                                                                      text_col = '#333333'))

g2 <- 
  populations %>% 
  filter(income.class == 'BR') %>% 
  group_by(hex) %>% 
  reframe(n = n()) %>% 
  left_join(aop) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(data = aop, fill = 'black', color = NA) +
  geom_sf(aes(fill = n), color = NA) +
  theme_gtfswizard +
  viridis::scale_fill_viridis(option = 'H', labels = scales::label_number(big.mark = ",")) +
  labs(fill = 'Low-income\npopulation', title = 'a) Travel production') +
  ggspatial::annotation_scale(location = "bl", text_cex = 0.8, bar_cols = c("#333333", 'white')) +
  ggspatial::annotation_north_arrow(location = "tr",
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c('white', '#333333'),
                                                                                      line_col = '#333333',
                                                                                      text_col = '#333333'))
grob <- gridExtra::arrangeGrob(g2, g1, ncol = 2)
ggsave('figs/1. production atraction2.png', grob, dpi = 320, height = 5, width = 14)

# TTM ----
list.files('data/r5rcore', 'zip', full.names = T) %>% 
  unlink()

final.gtfs <- 
  merge_gtfs(bus.gtfs, metro.gtfs)

stop_r5()
write_gtfs(final.gtfs, 'data/r5rcore/gtfs.zip')
r5rcore <- setup_r5('data/r5rcore', overwrite = TRUE)

final.gtfs$calendar$end_date # data da simulacao

ttm <-
  travel_time_matrix(
    r5r_core = r5rcore,
    origins = origins,
    destinations = destinations,
    progress = TRUE,
    mode = 'TRANSIT',
    departure_datetime = dmy_hms("13/12/2021 06:30:00"),
    time_window = 60,
    percentiles = 50, # decidir com moraes e justificar
    max_walk_time = 15,
    max_trip_duration = 120,
    draws_per_minute = 1
  ) %>% 
  tibble %>% 
  setNames(c('from_id', 'to_id', 'travel_time'))

data.table::fwrite(ttm, 'data/performance/ttm/ttm.donothing.csv')
  
# do nothing accessibility ----
ttm <- 
  data.table::fread('data/performance/ttm/ttm.baseline.csv')

od <- 
  tibble(from_id = origins$id) %>% 
  group_by(from_id) %>% 
  reframe(to_id = destinations$id) %>% 
  filter(!from_id == to_id)

sigma <- 50.95931 # 50% dos empregos alcançáveis em 60 minutos
rvmethod::gaussfunc(60, 0, 50.95931)

cum.jobs <- 
  left_join(od, ttm) %>% 
  mutate(travel_time = if_else(is.na(travel_time), Inf, travel_time)) %>% 
  filter(!from_id == to_id) %>% 
  rename(hex = to_id) %>% 
  left_join(jobs) %>% 
  na.omit() %>% 
  group_by(from_id, travel_time, job.class) %>% 
  reframe(jobs = sum(jobs)) %>%
  mutate(dec.jobs = jobs * rvmethod::gaussfunc(travel_time, 0, sigma)) %>%
  rename(hex = from_id) %>% 
  group_by(hex, job.class) %>% 
  reframe(cum.jobs = sum(dec.jobs)) %>% 
  mutate(cum.jobs = if_else(cum.jobs == 0, min(cum.jobs[cum.jobs != 0])/2, cum.jobs),
         income.class = if_else(job.class == "jobs.high", 'AR', if_else(job.class == 'jobs.med', 'MR', 'BR')))

min.wage <- 1518
cost <- 20 * 4.5 * 2
i.budget <- .1

tot.jobs <- 
  jobs %>% 
  group_by(job.class) %>% 
  reframe(tot.jobs = sum(jobs, na.rm = T)) %>% 
  mutate(income.class = c('AR', 'BR', 'MR')) %>% 
  select(-job.class)

accessibility <- 
  populations %>% 
  left_join(cum.jobs) %>% 
  mutate(cum.jobs = if_else(is.na(cum.jobs), 0, cum.jobs),
         Rend_pc = Rend_pc * min.wage,
         transport.budget = if_else(work.class == 'formal' | age >= 60, 1,
                                    if_else(student == 'student', i.budget*Rend_pc*3/cost, i.budget*Rend_pc/cost)),
         transport.budget = if_else(transport.budget > 1, 1, transport.budget)) %>% 
    left_join(tot.jobs) %>% 
    mutate(tot.jobs = if_else(is.na(tot.jobs), 0, tot.jobs),
      accessibility = (cum.jobs * transport.budget) / tot.jobs)

accessibility %>% 
  fwrite('data/performance/donothing.access.csv')
beepr::beep(4)

ggplot() +
  geom_sf(data = shp.bairro, color = NA) +
  geom_sf(data = accessibility %>%
            group_by(hex) %>% 
            reframe(accessibility = mean(accessibility, na.rm = T)) %>% 
            left_join(aop) %>% 
            st_as_sf(),
          aes(fill = accessibility), color = NA) +
  viridis::scale_fill_viridis(option = 'H', labels = scales::percent_format()) +
  theme_gtfswizard +
  ggspatial::annotation_scale(location = "bl", text_cex = 0.8, bar_cols = c("#333333", 'white')) +
  ggspatial::annotation_north_arrow(location = "tr",
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c('white', '#333333'),
                                                                                      line_col = '#333333',
                                                                                      text_col = '#333333')) 
ggplot() +
  geom_sf(data = shp.bairro, color = NA) +
  geom_sf(data = accessibility %>%
            mutate(accessibility = accessibility / mean(accessibility, na.rm = T)) %>% 
            group_by(hex) %>% 
            reframe(inequitable.access = mean(accessibility, na.rm = T)) %>% 
            left_join(aop) %>% 
            st_as_sf(),
          aes(fill = inequitable.access), color = NA) +
  scale_fill_gradient2(high = 'green4', low = 'firebrick', mid = 'white', midpoint = 1)+
  theme_gtfswizard +
  ggspatial::annotation_scale(location = "bl", text_cex = 0.8, bar_cols = c("#333333", 'white')) +
  ggspatial::annotation_north_arrow(location = "tr",
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c('white', '#333333'),
                                                                                      line_col = '#333333',
                                                                                      text_col = '#333333')) 
#ggsave('figs/baseline.scenario.png')

# critical accessibility ----
critical.access <- read_lines('data/critical.access.txt') %>% as.numeric()
  
accessibility <- 
  accessibility %>% 
  mutate(critical = if_else(accessibility < critical.access, 'critical', 'non.critical') %>% factor(levels = c('critical', 'non.critical')),
         problematic = if_else(income.class == 'BR' & critical == 'critical', 'problematic', 'non.problematic') %>% factor(levels = c('problematic', 'non.problematic')))

# problem magnitude ---------------------------------------------------------------------------
U_i <- fread('data/performance/baseline.problem.csv') %>% 
  select(id, U_i)

donothing.problem <- 
  accessibility %>% 
  select(hex, income.class, id, accessibility, problematic) %>% 
  left_join(U_i) %>% 
  mutate(p_i = problematic == 'problematic',
         problem = p_i/U_i)

donothing.problem %>% 
  data.table::fwrite('data/performance/donothing.problem.csv')

donothing.problem %>% 
  group_by(hex) %>% 
  reframe(problem = sum(problem)) %>% 
  left_join(aop, by = join_by(hex)) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(data = shp.bairro) +
  geom_sf(aes(fill = problem), color = NA) +
  theme_gtfswizard +
  viridis::scale_fill_viridis(option = 'H', labels = scales::comma_format()) +
  #scale_fill_gradient2(low = 'firebrick', mid = 'white', high = 'green4', midpoint = 1) +
  #labs(fill = 'U') +
  ggspatial::annotation_scale(location = "bl", text_cex = 0.8, bar_cols = c("#333333", NA)) +
  ggspatial::annotation_north_arrow(location = "tr", style = ggspatial::north_arrow_fancy_orienteering(fill = c(NA, '#333333'), line_col = '#333333', text_col = '#333333'))
ggsave('figs/3. donothing.problem.map.png', dpi = 320, height = 5)

tibble(
  scenario = 'donothing',
  prob.mag = donothing.problem$problem %>% sum,
  bus.fleet = GTFSwizard::get_fleet(bus.gtfs, method = 'by.hour') %>% 
    .$fleet %>% 
    max,
  bus.vkt = GTFSwizard::get_distances(bus.gtfs, method = 'by.route') %>% 
    reframe(total.distance = trips * average.distance) %>% 
    .$total.distance %>% 
    sum,
  metro.fleet = GTFSwizard::get_fleet(metro.gtfs, method = 'by.hour') %>% # adicionar filter_time
    .$fleet %>% 
    max,
  metro.vkt = GTFSwizard::get_distances(metro.gtfs, method = 'by.route') %>% # adicionar filter_time
    reframe(total.distance = trips * average.distance) %>% 
    .$total.distance %>% 
    sum
) %>% 
  write_csv('data/performance/donothing.scenario.performance.csv')