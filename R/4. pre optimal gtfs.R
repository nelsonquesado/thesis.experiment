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

options(java.parameters = '-Xmx7G')
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

# metro data ----
metro.gtfs.file <- 'data/gtfs/4.b metro.future.zip'
metro.gtfs <- read_gtfs(metro.gtfs.file) %>% 
  get_shapes()

metro.gtfs$stops$stop_id <- paste0('M', metro.gtfs$stops$stop_id)
metro.gtfs$stop_times$stop_id <- paste0('M', metro.gtfs$stop_times$stop_id)

# start of super gtfs -------------------------------------------------------------------------
bus.gtfs.file <- 'data/gtfs/1.b bus.baseline.zip'
bus.gtfs <- read_gtfs(bus.gtfs.file)

# headways to reduce --------------------------------------------------------------------------
trips.to.double <- 
  get_headways(bus.gtfs, 'by.trip') %>% 
  filter(headway_minutes > 60) %>% 
  mutate(delay = (headway_minutes / 2) * 60) %>% 
  .[, c(2, 7)]

bus.baseline.extra <-
  filter_trip(bus.gtfs, trip = trips.to.double$trip_id, keep = TRUE)

for (i in 1:nrow(trips.to.double)) {
  
  bus.baseline.extra <- 
    delay_trip(bus.baseline.extra, trip = trips.to.double$trip_id[i], duration = trips.to.double$delay[i])
  
}

rm(i, trips.to.double)

bus.baseline.extra <- merge_gtfs(bus.gtfs, bus.baseline.extra)

# secctionamento ------------------------------------------------------------------------------
trips.to.split <- 
  bus.baseline.extra$stop_times$trip_id %>% 
  table %>% 
  as_tibble() %>% 
  mutate(perc = percent_rank(n)) %>% 
  arrange(n) %>% 
  filter(perc >= .8) %>% 
  .$.

bus.baseline.split <- 
  bus.baseline.extra %>% 
  filter_trip(trips.to.split, keep = T) %>% 
  split_trip(trip = trips.to.split, split = 1)

bus.baseline.split <- 
  bus.baseline.split %>% 
  delay_trip(trip = bus.baseline.split$trips$trip_id[str_detect(bus.baseline.split$trips$trip_id, '\\.B')], duration = 30)

rm(trips.to.split)

pre.super.gtfs <-
  GTFSwizard::merge_gtfs(bus.baseline.extra, bus.baseline.split)

# corridors -----------------------------------------------------------------------------------
plot_corridor(pre.super.gtfs, i = .085, min.length = 2000) 
ggsave('figs/corridors.png', dpi = 320, width = 6, height = 4.5)

corridors <- 
  get_corridor(pre.super.gtfs, i = .085, min.length = 2000) 

corridors$length %>% sum()

trips <- corridors$trip_id %>% unlist %>% unique
stops <- corridors$stop_id %>% unlist %>% unique

pre.super.speed.gtfs <-
  edit_speed(pre.super.gtfs, trips, stops, factor = 1.1)

rm(bus.gtfs, bus.baseline.extra, bus.baseline.split, pre.super.gtfs)

# TTM ----------------------------------------------------------------------------------------
list.files('data/r5rcore', 'zip', full.names = T) %>% 
  unlink()

stop_r5()
write_gtfs(pre.super.speed.gtfs, 'data/r5rcore/gtfs.bus.zip')
write_gtfs(metro.gtfs, 'data/r5rcore/gtfs.metro.zip')
r5rcore <- setup_r5('data/r5rcore', overwrite = TRUE)

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
beepr::beep()

# pre optimal accessibility ----
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

# critical accessibility ----
critical.access <- read_lines('data/critical.access.txt') %>% as.numeric()

accessibility <- 
  accessibility %>% 
  mutate(critical = if_else(accessibility < critical.access, 'critical', 'non.critical') %>% factor(levels = c('critical', 'non.critical')),
         problematic = if_else(income.class == 'BR' & critical == 'critical', 'problematic', 'non.problematic') %>% factor(levels = c('problematic', 'non.problematic')))

# problem magnitude ---------------------------------------------------------------------------
U_i <- fread('data/performance/baseline.problem.csv') %>% 
  select(id, U_i)

preoptimal.problem <- 
  accessibility %>% 
  select(hex, income.class, id, accessibility, problematic) %>% 
  left_join(U_i) %>% 
  mutate(p_i = problematic == 'problematic',
         problem = p_i/U_i)

preoptimal.problem %>% 
  data.table::fwrite('data/performance/preoptimal.problem.csv')

preoptimal.problem %>% 
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
ggsave('figs/3. preoptimal.problem.map.png', dpi = 320, height = 5)

tibble(
  scenario = 'preoptimal',
  prob.mag = preoptimal.problem$problem %>% sum,
  bus.fleet = GTFSwizard::get_fleet(pre.super.speed.gtfs, method = 'by.hour') %>% 
    .$fleet %>% 
    max,
  bus.vkt = GTFSwizard::get_distances(pre.super.speed.gtfs, method = 'by.route') %>% 
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
  write_csv('data/performance/preoptimal.scenario.performance.csv')

# express service -----------------------------------------------------------------------------
stops.sf <- get_stops_sf(pre.super.speed.gtfs$stops)

critical.destinations <- 
  jobs %>% 
  filter(job.class == 'jobs.low') %>% 
  filter(percent_rank(jobs) >= .99) %>% 
  mutate(geom = st_centroid(geom))

critical.origins <- 
  preoptimal.problem %>% 
  group_by(hex) %>% 
  reframe(problem = sum(problem)) %>% 
  filter(percent_rank(problem) >= .99) %>% 
  select(hex) %>% 
  left_join(aop) %>% 
  st_as_sf() %>% 
  mutate(geom = st_centroid(geom))
    
stops.origin <- stops.sf$stop_id[st_nearest_feature(critical.origins, stops.sf)]
stops.destination <- stops.sf$stop_id[st_nearest_feature(critical.destinations, stops.sf)]

ggplot() +
  geom_sf(data = shp.bairro, fill = NA) +
  geom_sf(data = filter(stops.sf, stop_id %in% stops.destination), aes(color = 'destination')) +
  geom_sf(data = filter(stops.sf, stop_id %in% stops.origin), aes(color = 'origin')) +
  theme_gtfswizard +
  theme(legend.title = element_blank()) +
  ggspatial::annotation_scale(location = "bl", text_cex = 0.8, bar_cols = c("#333333", NA)) +
  ggspatial::annotation_north_arrow(location = "tr", style = ggspatial::north_arrow_fancy_orienteering(fill = c(NA, '#333333'), line_col = '#333333', text_col = '#333333'))

  
nodes <- c(stops.destination, stops.origin) %>% unique

# express $trips
pre.super.speed.gtfs$trips <- 
  pre.super.speed.gtfs$trips %>% 
  bind_rows(
    tibble(
      route_id = rep('express', each = 64),
      service_id = 'U.x.x',
      trip_id = paste0(rep('express.i', each = 64), '.T', 1:64),
      trip_headsign = "",
      trip_short_name = "",
      direction_id = 0,
      block_id = "",
      wheelchair_accessible = 2,
      shape_id = ""
    )
  )

pre.super.speed.gtfs$trips <- 
  pre.super.speed.gtfs$trips %>% 
  bind_rows(
    tibble(
      route_id = rep('express', each = 64),
      service_id = 'U.x.x',
      trip_id = paste0(rep('express.o', each = 64), '.T', 1:64),
      trip_headsign = "",
      trip_short_name = "",
      direction_id = 1,
      block_id = "",
      wheelchair_accessible = 2,
      shape_id = ""
    )
  )

# express $routes
pre.super.speed.gtfs$routes <- 
  pre.super.speed.gtfs$routes %>% 
  bind_rows(
    tibble(
      route_id = 'express',
      agency_id = '1.x.x',
      route_short_name = 'express',
      route_long_name = 'express',
      route_desc = "",
      route_type = 3,
      route_url = "",
      route_color = "",
      route_text_color = ""
    )
  )

# express $fare_rules
pre.super.speed.gtfs$fare_rules <- 
  pre.super.speed.gtfs$fare_rules %>% 
  bind_rows(
    tibble(
      fare_id = "1.x.x",
      route_id = 'express',
      origin_id = "",
      destination_id = "",
      contains_id = ""
    )
  )

# express $stop_times
durations <-
  pre.super.speed.gtfs %>%
  set_dwelltime(duration = 20) %>% 
  edit_dwelltime(factor = 0) %>%
  get_durations(method = 'detailed') %>% 
  group_by(from_stop_id, to_stop_id) %>% 
  reframe(duration = mean(duration, na.rm = T)) %>% 
  filter(duration >= 0)

graph <-
  graph_from_data_frame(durations, directed = FALSE)

E(graph)$weight <- E(graph)$duration

D <- distances(graph, v = nodes, to = nodes, weights = E(graph)$weight)

tsp_instance <- TSP(D)

route <- solve_TSP(tsp_instance)
route

# Extract the tour order as indices and then get the labels
order <- as.integer(route)
ordered_stops <- labels(tsp_instance)[order]

# Complete the cycle by appending the first stop at the end
ordered_stops_cycle <- c(ordered_stops, ordered_stops[1])

# Calculate the duration between consecutive stops
durations_vec <- sapply(seq_along(ordered_stops), function(i) {
  D[ordered_stops_cycle[i], ordered_stops_cycle[i+1]]
})

# Create a tibble with stop_id, sequence, and duration to the next stop
result <-
  tibble(
    stop_id = ordered_stops,
    stop_sequence = seq_along(ordered_stops),
    duration_to_next_stop = durations_vec
  )

# express $stop_times
# inbound
pre.super.speed.gtfs$stop_times <- 
  pre.super.speed.gtfs$stop_times %>% 
  bind_rows(
    result %>%
      bind_rows(.[1,]) %>% 
      mutate(stop_sequence = 1:n(),
             cum_duration_to_next_stop = cumsum(duration_to_next_stop)) %>% 
      mutate(arrival_time = 5400 + lag(cum_duration_to_next_stop) %>% if_else(is.na(.), 0, .) %>% round,
             departure_time = arrival_time) %>% 
      mutate(trip_id = list(paste0(rep('express.i', each = 64), '.T', 1:64)),
             trip_seq = list(0:63)) %>% 
      unnest(cols = c('trip_id', 'trip_seq')) %>% 
      arrange(trip_seq) %>% 
      mutate(delay = trip_seq * 300,
             departure_time = departure_time + delay,
             arrival_time = arrival_time + delay) %>% 
      filter(arrival_time >= 21600 | arrival_time >= 21600) %>% 
      filter(arrival_time <= 36000 | arrival_time <= 36000) %>% 
      mutate(departure_time = hms::hms(departure_time) %>% as.character(),
             arrival_time = hms::hms(arrival_time) %>% as.character(),
             stop_headsign ="" ,
             pickup_type = NA,
             drop_off_type = NA,
             shape_dist_traveled = NA) %>% 
      select(colnames(pre.super.speed.gtfs$stop_times))
  )

#outbound
pre.super.speed.gtfs$stop_times <-
  pre.super.speed.gtfs$stop_times %>%
  bind_rows(
    result %>%
      arrange(-stop_sequence) %>%
      bind_rows(.[1,]) %>%
      mutate(stop_sequence = 1:n(),
             duration_to_next_stop = lead(duration_to_next_stop) %>% if_else(is.na(.), 0, .),
             cum_duration_to_next_stop = cumsum(duration_to_next_stop)) %>% 
      mutate(arrival_time = 5400 + lag(cum_duration_to_next_stop) %>% if_else(is.na(.), 0, .) %>% round,
             departure_time = arrival_time) %>% 
      mutate(trip_id = list(paste0(rep('express.o', each = 64), '.T', 1:64)),
             trip_seq = list(0:63)) %>% 
      unnest(cols = c('trip_id', 'trip_seq')) %>% 
      arrange(trip_seq) %>% 
      mutate(delay = trip_seq * 300,
             departure_time = departure_time + delay,
             arrival_time = arrival_time + delay) %>% 
      filter(arrival_time >= 21600 | arrival_time >= 21600) %>% 
      filter(arrival_time <= 36000 | arrival_time <= 36000) %>% 
      mutate(departure_time = hms::hms(departure_time) %>% as.character(),
             arrival_time = hms::hms(arrival_time) %>% as.character(),
             stop_headsign ="" ,
             pickup_type = NA,
             drop_off_type = NA,
             shape_dist_traveled = NA) %>% 
      select(colnames(pre.super.speed.gtfs$stop_times))
  )

pre.super.speed.gtfs <- 
  pre.super.speed.gtfs %>% 
  get_shapes()

pre.super.speed.gtfs <- 
  pre.super.speed.gtfs %>% 
  edit_speed(trips = pre.super.speed.gtfs$trips %>% filter(route_id == 'express') %>% .$trip_id,
             factor = 1.2)

pre.super.speed.gtfs %>% 
  write_gtfs('data/gtfs/5. bus.pre.optimal.zip')

beepr::beep()

# TTM 2 ----------------------------------------------------------------------------------------
pre.super.speed.gtfs <- 
  read_gtfs('data/gtfs/5. bus.pre.optimal.zip')


list.files('data/r5rcore', 'zip', full.names = T) %>% 
  unlink()

stop_r5()
write_gtfs(pre.super.speed.gtfs, 'data/r5rcore/gtfs.bus.zip')
write_gtfs(metro.gtfs, 'data/r5rcore/gtfs.metro.zip')
r5rcore <- setup_r5('data/r5rcore', overwrite = TRUE)

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

# pre optimal 2 accessibility ----
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

# critical accessibility 2 ----
critical.access <- read_lines('data/critical.access.txt') %>% as.numeric()

accessibility <- 
  accessibility %>% 
  mutate(critical = if_else(accessibility < critical.access, 'critical', 'non.critical') %>% factor(levels = c('critical', 'non.critical')),
         problematic = if_else(income.class == 'BR' & critical == 'critical', 'problematic', 'non.problematic') %>% factor(levels = c('problematic', 'non.problematic')))

# problem 2 magnitude ---------------------------------------------------------------------------
U_i <- fread('data/performance/baseline.problem.csv') %>% 
  select(id, U_i)

preoptimal.problem <- 
  accessibility %>% 
  select(hex, income.class, id, accessibility, problematic) %>% 
  left_join(U_i) %>% 
  mutate(p_i = problematic == 'problematic',
         problem = p_i/U_i)

preoptimal.problem %>% 
  data.table::fwrite('data/performance/preoptimal.express.problem.csv')

preoptimal.problem %>% 
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
ggsave('figs/3. preoptimal.express.problem.map.png', dpi = 320, height = 5)

tibble(
  scenario = 'preoptimal2',
  prob.mag = preoptimal.problem$problem %>% sum,
  bus.fleet = GTFSwizard::get_fleet(pre.super.speed.gtfs, method = 'by.hour') %>% 
    .$fleet %>% 
    max,
  bus.vkt = GTFSwizard::get_distances(pre.super.speed.gtfs, method = 'by.route') %>% 
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
  write_csv('data/performance/preoptimal.express.scenario.performance.csv')

beepr::beep()
