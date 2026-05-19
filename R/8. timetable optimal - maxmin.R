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
library(parallel)

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

# super gtfs -------------------------------------------------------------------------
bus.gtfs.file <- 'data/gtfs/5. bus.pre.optimal.zip'
bus.gtfs <- read_gtfs(bus.gtfs.file)

# scenarios -----------------------------------------------------------------------------------
optimist.subsidy <- 
  fread('data/subsidy.scenarios.it-4.csv') %>% 
  mutate(id = paste0('f', fare, 's', subsidy)) %>% 
  select(id, fare, new.vkt)

scenarios <- optimist.subsidy$id

trip.distances <- 
  get_distances(bus.gtfs, method = 'by.trip') %>% 
  select(route_id, trip_id, distance) %>% 
  arrange(-distance)

trips <- trip.distances$trip_id

# gen 1 population ------------------------------------------------------------------------------------
first.initial.guess <- 
  trip.distances %>% 
  mutate(cum.distance = cumsum(distance) %>% as.numeric) %>%
  bind_cols(optimist.subsidy %>% 
              select(id, new.vkt) %>% 
              pivot_wider(names_from = id, values_from = new.vkt)) %>% 
  mutate_at(5, function(x){x >= .$cum.distance}) %>% 
  select(5) %>% 
  mutate_all(as.numeric)

guessfit <- 
  function(x, budget.vkt){
    proposal <- 
      trip.distances[if_else(x == 1, T, F), ]
    
    if(as.numeric(sum(proposal$distance)) > as.numeric(budget.vkt)) {
      
      cost <- 0
      
    } else {  
      
      cost <- sum(proposal$distance)
      
    }
    
    return(cost)
  }

second.guess <- 
  function(x){
    
    budget.vkt <- optimist.subsidy %>% filter(id == x) %>% .$new.vkt
    
    proposition <- 
      first.initial.guess %>% 
      select(all_of(x))
    
    popSize <- 100
    elitism <- 25
    
    guessol <-
      ga(type = "binary",
         fitness = guessfit,
         nBits = nrow(trip.distances), 
         popSize = popSize,
         maxiter = 10000,
         pmutation = .8,
         pcrossover = .8,
         elitism = elitism,
         suggestions = t(proposition),
         names = trip.distances$trip_id,
         budget.vkt = budget.vkt
      )
    
    return(guessol)
    
  }

guessol <- second.guess(scenarios)

guessol %>% 
  saveRDS('data/performance/ttm/timetable optimal/gen.1.rds')

# ttm gen.1 -----------------------------------------------------------------------------------
beepr::beep()
guessol <- 
  read_rds('data/performance/ttm/timetable optimal/gen.1.rds')

gen.1 <-
  guessol[[1]]@population %>% 
  rbind %>% 
  as_tibble() %>% 
  mutate(.before = 1,
         id = scenarios)

trips <- trip.distances$trip_id

service.list <- function(x){trips[gen.1 %>% select(-id) %>% .[x, ] == 1]}

calendar <- 
  tibble(trip_id = lapply(1:nrow(gen.1), service.list),
         monday = 1 %>% as.integer(),
         tuesday = 1 %>% as.integer(),
         wednesday = 1 %>% as.integer(),
         thursday = 1 %>% as.integer(),
         friday = 1 %>% as.integer(),
         saturday = 1 %>% as.integer(),
         sunday = 1 %>% as.integer(),
         start_date = lubridate::as_datetime('2021-12-12') + (86400 * 1:nrow(gen.1)), 
         end_date = start_date,
         service_id = paste0('scn', 1:nrow(gen.1))
  )

bus.gtfs$calendar <-calendar[, -1]

calendar.unnest <- 
  calendar[, c(1, 11)] %>% 
  unnest(trip_id) %>% 
  mutate(new_trip_id = 1:nrow(.) %>% as.character())

bus.gtfs$trips <-
  calendar.unnest %>% 
  left_join(bus.gtfs$trips %>% 
              select(-service_id),
            by = 'trip_id') %>% 
  select(-trip_id) %>% 
  rename(trip_id = new_trip_id)

bus.gtfs$stop_times <- 
  calendar.unnest[, -2] %>% 
  left_join(bus.gtfs$stop_times) %>% 
  select(-trip_id) %>% 
  rename(trip_id = new_trip_id)

trip.distances <- 
  get_distances(bus.gtfs, method = 'by.trip') %>% 
  select(trip_id, distance)

r5r::stop_r5()

beepr::beep()

GTFSwizard::write_gtfs(bus.gtfs, 'data/r5rcore/gtfs.bus.zip')
GTFSwizard::write_gtfs(metro.gtfs, 'data/r5rcore/gtfs.metro.zip')

r5rcore <-
  setup_r5('data/r5rcore', overwrite = TRUE)

departures <-
  ymd_hms(unique(calendar$start_date) + 25200)

ttm_func <- 
  function(x) {
    
    travel_time_matrix(
      r5r_core = r5rcore,
      origins = origins,
      destinations = destinations,
      progress = FALSE,
      mode = 'TRANSIT',
      departure_datetime = x,
      time_window = 45,
      percentiles = 50, # decidir com moraes e justificar
      max_walk_time = 15,
      max_trip_duration = 90,
      n_threads = 7,
      draws_per_minute = 1) %>% 
      tibble %>% 
      setNames(c('from_id', 'to_id', 'travel_time')) %>% 
      data.table::fwrite(paste0("data/performance/ttm/timetable optimal - maximin/gen.1/", janitor::make_clean_names(x), '.csv'))
    
    gc()
    
  }

lapply(departures, ttm_func)

r5r::stop_r5()

# gen 1 accessibilidade -----------------------------------------------------------------------
critical.access <- read_lines('data/critical.access.txt') %>% as.numeric()

gen.1.files <- list.files('data/performance/ttm/timetable optimal - maximin/gen.1/', '.csv', full.names = T)

min.wage <- 1518
cost <- 20 * 4.5 * 2
i.budget <- .1

tot.jobs <- 
  jobs %>% 
  group_by(job.class) %>% 
  reframe(tot.jobs = sum(jobs, na.rm = T)) %>% 
  mutate(income.class = c('AR', 'BR', 'MR')) %>% 
  select(-job.class)

od <- 
  tibble(from_id = origins$id) %>% 
  group_by(from_id) %>% 
  reframe(to_id = destinations$id) %>% 
  filter(!from_id == to_id)

sigma <- 50.95931 # 50% dos empregos alcançáveis em 60 minutos
rvmethod::gaussfunc(60, 0, 50.95931)

U_i <- fread('data/performance/baseline.problem.csv') %>% 
  select(id, U_i)
  
cost.BR <- 0

#index2access(i)

#i <- 7
index2access <- function(i) {
  
  ttm <- fread(gen.1.files[i])
  
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
  
  accessibility <- 
    populations %>% 
    left_join(cum.jobs) %>% 
    mutate(cum.jobs = if_else(is.na(cum.jobs), 0, cum.jobs),
           Rend_pc = Rend_pc * min.wage,
           transport.budget = if_else(work.class == 'formal' | age >= 60, 1,
                                      if_else(student == 'student', i.budget*Rend_pc*3/cost,
                                              if_else(income.class == 'BR', i.budget * Rend_pc/cost.BR %>% if_else(cost.BR == 0, 1, .), i.budget*Rend_pc/cost))),
           transport.budget = if_else(transport.budget > 1, 1, transport.budget)) %>% 
    left_join(tot.jobs) %>% 
    mutate(tot.jobs = if_else(is.na(tot.jobs), 0, tot.jobs),
           accessibility = (cum.jobs * transport.budget) / tot.jobs)
  
  problem <- 
    accessibility %>% 
    filter(income.class == 'BR') %>% 
    .$accessibility %>% 
    min(., na.rm = T)
  
  return(problem)
  
}

tic <- Sys.time()
tot.problem.gen.1 <- lapply(1:100, index2access)
tac <- Sys.time()
tac - tic
beepr::beep()

saveRDS(tot.problem.gen.1, 'data/performance/ttm/timetable optimal - maximin/gen.1/tot.problem.gen.1.rds')

# gen 2 population ------------------------------------------------------------------------------------
guessol <- 
  read_rds('data/performance/ttm/timetable optimal - maximin/gen.1.rds')

elitism.manual <- 75

proposition <-
  guessol[[1]]@population %>% 
  rbind %>% 
  as_tibble() %>% 
    .[read_rds('data/performance/ttm/timetable optimal - maximin/gen.1/tot.problem.gen.1.rds') %>% unlist() %>% order > elitism.manual, ]

budget.vkt <- optimist.subsidy$new.vkt

#x <- t(proposition)[, 1]

guessfit <- 
  function(x){
    proposal <- 
      trip.distances[if_else(x == 1, T, F), ]
    
    if(as.numeric(sum(proposal$distance)) >= as.numeric(budget.vkt)) {
      
      cost <- 0
      
    } else {  
      
      cost <- sum(proposal$distance)
      
    }
    
    return(cost)
  }

popSize <- 100
elitism <- 25

guessol <-
  ga(type = "binary",
     fitness = guessfit,
     nBits = nrow(trip.distances), 
     popSize = popSize,
     maxiter = 10000,
     pmutation = .8,
     pcrossover = .8,
     elitism = elitism,
     suggestions = proposition,
     names = trip.distances$trip_id
  )

guessol %>% 
  saveRDS('data/performance/ttm/timetable optimal - maximin/gen.2.rds')

# ttm gen.2 -----------------------------------------------------------------------------------
beepr::beep()
guessol <- 
  read_rds('data/performance/ttm/timetable optimal - maximin/gen.2.rds')

gen.2 <-
  guessol@population %>% 
  rbind %>% 
  as_tibble()

service.list <- function(x){trips[gen.2[x, ] == 1]}

calendar <- 
  tibble(trip_id = lapply(1:nrow(gen.2), service.list),
         monday = 1 %>% as.integer(),
         tuesday = 1 %>% as.integer(),
         wednesday = 1 %>% as.integer(),
         thursday = 1 %>% as.integer(),
         friday = 1 %>% as.integer(),
         saturday = 1 %>% as.integer(),
         sunday = 1 %>% as.integer(),
         start_date = lubridate::as_datetime('2021-12-12') + (86400 * 1:nrow(gen.2)), 
         end_date = start_date,
         service_id = paste0('scn', 1:nrow(gen.2))
  )

bus.gtfs$calendar <-calendar[, -1]

calendar.unnest <- 
  calendar[, c(1, 11)] %>% 
  unnest(trip_id) %>% 
  mutate(new_trip_id = 1:nrow(.) %>% as.character())

bus.gtfs$trips <-
  calendar.unnest %>% 
  left_join(bus.gtfs$trips %>% 
              select(-service_id),
            by = 'trip_id') %>% 
  select(-trip_id) %>% 
  rename(trip_id = new_trip_id)

bus.gtfs$stop_times <- 
  calendar.unnest[, -2] %>% 
  left_join(bus.gtfs$stop_times, 
            relationship = 'many-to-many') %>% 
  select(-trip_id) %>% 
  rename(trip_id = new_trip_id)

trip.distances <- 
  get_distances(bus.gtfs, method = 'by.trip') %>% 
  select(trip_id, distance)

r5r::stop_r5()


GTFSwizard::write_gtfs(bus.gtfs, 'data/r5rcore/gtfs.bus.zip')
GTFSwizard::write_gtfs(metro.gtfs, 'data/r5rcore/gtfs.metro.zip')

beepr::beep()

r5rcore <-
  setup_r5('data/r5rcore', overwrite = TRUE)

departures <-
  ymd_hms(unique(calendar$start_date) + 25200)

ttm_func <- 
  function(x) {
    
    travel_time_matrix(
      r5r_core = r5rcore,
      origins = origins,
      destinations = destinations,
      progress = FALSE,
      mode = 'TRANSIT',
      departure_datetime = x,
      time_window = 45,
      percentiles = 50, # decidir com moraes e justificar
      max_walk_time = 15,
      max_trip_duration = 90,
      n_threads = 7,
      draws_per_minute = 1) %>% 
      tibble %>% 
      setNames(c('from_id', 'to_id', 'travel_time')) %>% 
      data.table::fwrite(paste0("data/performance/ttm/timetable optimal - maximin/gen.2/", janitor::make_clean_names(x), '.csv'))
    
    gc()
    
  }

lapply(departures, ttm_func)

r5r::stop_r5()

# gen 2 accessibilidade ----------------------------------------------------------------------- PAREI AQUI
critical.access <- read_lines('data/critical.access.txt') %>% as.numeric()

gen.2.files <- list.files('data/performance/ttm/timetable optimal - maximin/gen.2/', '.csv', full.names = T)

min.wage <- 1518
cost <- 20 * 4.5 * 2
i.budget <- .1

tot.jobs <- 
  jobs %>% 
  group_by(job.class) %>% 
  reframe(tot.jobs = sum(jobs, na.rm = T)) %>% 
  mutate(income.class = c('AR', 'BR', 'MR')) %>% 
  select(-job.class)

od <- 
  tibble(from_id = origins$id) %>% 
  group_by(from_id) %>% 
  reframe(to_id = destinations$id) %>% 
  filter(!from_id == to_id)

sigma <- 50.95931 # 50% dos empregos alcançáveis em 60 minutos
rvmethod::gaussfunc(60, 0, 50.95931)

U_i <- fread('data/performance/baseline.problem.csv') %>% 
  select(id, U_i)

cost.BR <- 0

#index2access(i)
#i <- 7
index2access <- function(i) {
  
  ttm <- fread(gen.2.files[i])
  
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
  
  accessibility <- 
    populations %>% 
    left_join(cum.jobs) %>% 
    mutate(cum.jobs = if_else(is.na(cum.jobs), 0, cum.jobs),
           Rend_pc = Rend_pc * min.wage,
           transport.budget = if_else(work.class == 'formal' | age >= 60, 1,
                                      if_else(student == 'student', i.budget*Rend_pc*3/cost,
                                              if_else(income.class == 'BR', i.budget * Rend_pc/cost.BR %>% if_else(cost.BR == 0, 1, .), i.budget*Rend_pc/cost))),
           transport.budget = if_else(transport.budget > 1, 1, transport.budget)) %>% 
    left_join(tot.jobs) %>% 
    mutate(tot.jobs = if_else(is.na(tot.jobs), 0, tot.jobs),
           accessibility = (cum.jobs * transport.budget) / tot.jobs)
  
  accessibility <- 
    accessibility %>% 
    mutate(critical = if_else(accessibility < critical.access, 'critical', 'non.critical') %>% factor(levels = c('critical', 'non.critical')),
           problematic = if_else(income.class == 'BR' & critical == 'critical', 'problematic', 'non.problematic') %>% factor(levels = c('problematic', 'non.problematic')))
  
  problem <- 
    accessibility %>% 
    select(hex, income.class, id, accessibility, problematic) %>% 
    left_join(U_i) %>% 
    mutate(p_i = problematic == 'problematic',
           problem = p_i/U_i)
  
  tot.problem <- problem$problem %>% sum
  
  return(tot.problem)
  
}

cl <- makeCluster(detectCores() - 1)

clusterExport(cl, varlist = c("index2access", "gen.2.files", "jobs", "origins", "od",
                              "destinations", "populations", "tot.jobs", "critical.access",
                              "min.wage", "cost", "i.budget", "sigma", "U_i", "cost.BR"))
# Load necessary libraries on each worker
clusterEvalQ(cl, {
  library(data.table)
  library(dplyr)
  library(rvmethod)
  library(GTFSwizard)
  library(tidyverse)
  library(aopdata)
  library(GA)
  library(sf)
  library(data.table)
  library(igraph)
  library(TSP)
  library(parallel)
})

tic <- Sys.time()
tot.problem.gen.2 <- parLapply(cl, 1:100, index2access)
stopCluster(cl)
tac <- Sys.time()
tac - tic
beepr::beep()

saveRDS(tot.problem.gen.2, 'data/performance/ttm/timetable optimal/gen.2/tot.problem.gen.2.rds')

# gen 2 accessibilidade -----------------------------------------------------------------------
critical.access <- read_lines('data/critical.access.txt') %>% as.numeric()

gen.2.files <- list.files('data/performance/ttm/timetable optimal - maximin/gen.2/', '.csv', full.names = T)

min.wage <- 1518
cost <- 20 * 4.5 * 2
i.budget <- .1

tot.jobs <- 
  jobs %>% 
  group_by(job.class) %>% 
  reframe(tot.jobs = sum(jobs, na.rm = T)) %>% 
  mutate(income.class = c('AR', 'BR', 'MR')) %>% 
  select(-job.class)

od <- 
  tibble(from_id = origins$id) %>% 
  group_by(from_id) %>% 
  reframe(to_id = destinations$id) %>% 
  filter(!from_id == to_id)

sigma <- 50.95931 # 50% dos empregos alcançáveis em 60 minutos
rvmethod::gaussfunc(60, 0, 50.95931)

U_i <- fread('data/performance/baseline.problem.csv') %>% 
  select(id, U_i)
  
cost.BR <- 0

#index2access(i)

#i <- 7
index2access <- function(i) {
  
  ttm <- fread(gen.2.files[i])
  
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
  
  accessibility <- 
    populations %>% 
    left_join(cum.jobs) %>% 
    mutate(cum.jobs = if_else(is.na(cum.jobs), 0, cum.jobs),
           Rend_pc = Rend_pc * min.wage,
           transport.budget = if_else(work.class == 'formal' | age >= 60, 1,
                                      if_else(student == 'student', i.budget*Rend_pc*3/cost,
                                              if_else(income.class == 'BR', i.budget * Rend_pc/cost.BR %>% if_else(cost.BR == 0, 1, .), i.budget*Rend_pc/cost))),
           transport.budget = if_else(transport.budget > 1, 1, transport.budget)) %>% 
    left_join(tot.jobs) %>% 
    mutate(tot.jobs = if_else(is.na(tot.jobs), 0, tot.jobs),
           accessibility = (cum.jobs * transport.budget) / tot.jobs)
  
  problem <- 
    accessibility %>% 
    filter(income.class == 'BR') %>% 
    .$accessibility %>% 
    min(., na.rm = T)
  
  return(problem)
  
}

tic <- Sys.time()
tot.problem.gen.2 <- lapply(1:100, index2access)
tac <- Sys.time()
tac - tic
beepr::beep()

saveRDS(tot.problem.gen.2, 'data/performance/ttm/timetable optimal - maximin/gen.2/tot.problem.gen.2.rds')

# gen 3 population ------------------------------------------------------------------------------------
guessol <- 
  read_rds('data/performance/ttm/timetable optimal - maximin/gen.2.rds')

elitism.manual <- 75

proposition <-
  guessol@population %>% 
  rbind %>% 
  as_tibble() %>% 
    .[read_rds('data/performance/ttm/timetable optimal - maximin/gen.2/tot.problem.gen.2.rds') %>% unlist() %>% order > elitism.manual, ]

budget.vkt <- optimist.subsidy$new.vkt

#x <- t(proposition)[, 1]

guessfit <- 
  function(x){
    proposal <- 
      trip.distances[if_else(x == 1, T, F), ]
    
    if(as.numeric(sum(proposal$distance)) >= as.numeric(budget.vkt)) {
      
      cost <- 0
      
    } else {  
      
      cost <- sum(proposal$distance)
      
    }
    
    return(cost)
  }

popSize <- 100
elitism <- 25

guessol <-
  ga(type = "binary",
     fitness = guessfit,
     nBits = nrow(trip.distances), 
     popSize = popSize,
     maxiter = 20000,
     pmutation = .8,
     pcrossover = .8,
     elitism = elitism,
     suggestions = proposition,
     names = trip.distances$trip_id
  )

guessol %>% 
  saveRDS('data/performance/ttm/timetable optimal - maximin/gen.3.rds')

# ttm gen.3 -----------------------------------------------------------------------------------
beepr::beep()
guessol <- 
  read_rds('data/performance/ttm/timetable optimal - maximin/gen.3.rds')

gen.3 <-
  guessol@population %>% 
  rbind %>% 
  as_tibble()

service.list <- function(x){trips[gen.3[x, ] == 1]}

calendar <- 
  tibble(trip_id = lapply(1:nrow(gen.3), service.list),
         monday = 1 %>% as.integer(),
         tuesday = 1 %>% as.integer(),
         wednesday = 1 %>% as.integer(),
         thursday = 1 %>% as.integer(),
         friday = 1 %>% as.integer(),
         saturday = 1 %>% as.integer(),
         sunday = 1 %>% as.integer(),
         start_date = lubridate::as_datetime('2021-12-12') + (86400 * 1:nrow(gen.3)), 
         end_date = start_date,
         service_id = paste0('scn', 1:nrow(gen.3))
  )

bus.gtfs$calendar <-calendar[, -1]

calendar.unnest <- 
  calendar[, c(1, 11)] %>% 
  unnest(trip_id) %>% 
  mutate(new_trip_id = 1:nrow(.) %>% as.character())

bus.gtfs$trips <-
  calendar.unnest %>% 
  left_join(bus.gtfs$trips %>% 
              select(-service_id),
            by = 'trip_id') %>% 
  select(-trip_id) %>% 
  rename(trip_id = new_trip_id)

bus.gtfs$stop_times <- 
  calendar.unnest[, -2] %>% 
  left_join(bus.gtfs$stop_times, 
            relationship = 'many-to-many') %>% 
  select(-trip_id) %>% 
  rename(trip_id = new_trip_id)

trip.distances <- 
  get_distances(bus.gtfs, method = 'by.trip') %>% 
  select(trip_id, distance)

GTFSwizard::write_gtfs(bus.gtfs, 'data/r5rcore/gtfs.bus.zip')
GTFSwizard::write_gtfs(metro.gtfs, 'data/r5rcore/gtfs.metro.zip')

beepr::beep()

r5rcore <-
  setup_r5('data/r5rcore', overwrite = TRUE)

departures <-
  ymd_hms(unique(calendar$start_date) + 25200)

ttm_func <- 
  function(x) {
    
    travel_time_matrix(
      r5r_core = r5rcore,
      origins = origins,
      destinations = destinations,
      progress = FALSE,
      mode = 'TRANSIT',
      departure_datetime = x,
      time_window = 45,
      percentiles = 50, # decidir com moraes e justificar
      max_walk_time = 15,
      max_trip_duration = 90,
      n_threads = 7,
      draws_per_minute = 1) %>% 
      tibble %>% 
      setNames(c('from_id', 'to_id', 'travel_time')) %>% 
      data.table::fwrite(paste0("data/performance/ttm/timetable optimal - maximin/gen.3/", janitor::make_clean_names(x), '.csv'))
    
    gc()
    
  }

lapply(departures, ttm_func)

# gen 3 accessibilidade -----------------------------------------------------------------------
critical.access <- read_lines('data/critical.access.txt') %>% as.numeric()

gen.3.files <- list.files('data/performance/ttm/timetable optimal - maximin/gen.3/', '.csv', full.names = T)

min.wage <- 1518
cost <- 20 * 4.5 * 2
i.budget <- .1

tot.jobs <- 
  jobs %>% 
  group_by(job.class) %>% 
  reframe(tot.jobs = sum(jobs, na.rm = T)) %>% 
  mutate(income.class = c('AR', 'BR', 'MR')) %>% 
  select(-job.class)

od <- 
  tibble(from_id = origins$id) %>% 
  group_by(from_id) %>% 
  reframe(to_id = destinations$id) %>% 
  filter(!from_id == to_id)

sigma <- 50.95931 # 50% dos empregos alcançáveis em 60 minutos

U_i <- fread('data/performance/baseline.problem.csv') %>% 
  select(id, U_i)
  
cost.BR <- 0

#index2access(i)

#i <- 7
index2access <- function(i) {
  
  ttm <- fread(gen.3.files[i])
  
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
  
  accessibility <- 
    populations %>% 
    left_join(cum.jobs) %>% 
    mutate(cum.jobs = if_else(is.na(cum.jobs), 0, cum.jobs),
           Rend_pc = Rend_pc * min.wage,
           transport.budget = if_else(work.class == 'formal' | age >= 60, 1,
                                      if_else(student == 'student', i.budget*Rend_pc*3/cost,
                                              if_else(income.class == 'BR', i.budget * Rend_pc/cost.BR %>% if_else(cost.BR == 0, 1, .), i.budget*Rend_pc/cost))),
           transport.budget = if_else(transport.budget > 1, 1, transport.budget)) %>% 
    left_join(tot.jobs) %>% 
    mutate(tot.jobs = if_else(is.na(tot.jobs), 0, tot.jobs),
           accessibility = (cum.jobs * transport.budget) / tot.jobs)
  
  problem <- 
    accessibility %>% 
    filter(income.class == 'BR') %>% 
    .$accessibility %>% 
    min(., na.rm = T)
  
  return(problem)
  
}

tic <- Sys.time()
tot.problem.gen.3 <- lapply(1:100, index2access)
tac <- Sys.time()
tac - tic
beepr::beep()

saveRDS(tot.problem.gen.3, 'data/performance/ttm/timetable optimal - maximin/gen.3/tot.problem.gen.3.rds')

# optimal solution ----------------------------------------------------------------------------
bus.gtfs.file <- 'data/gtfs/5. bus.pre.optimal.zip'

bus.gtfs <- read_gtfs(bus.gtfs.file)

optimal.trips <- 
  trips[gen.3[tot.problem.gen.3 %>% unlist %>% order == 1, ] %>% 
          t %>%
          c() == 1]

bus.gtfs <- 
  bus.gtfs %>% 
  filter_trip(optimal.trips)

bus.gtfs %>% 
  write_gtfs('data/gtfs/6. bus.optimal - maximin.zip')
