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

options(java.parameters = '-Xmx6G')
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

# scenarios -----------------------------------------------------------------------------------
optimist.subsidy <- 
  fread('data/subsidy.scenarios.csv') %>% 
  filter(subsidy == .35) %>% 
  mutate(id = paste0( LETTERS[1:n()], '.', subsidy) %>% 
           str_remove('0.')) %>% 
  select(id, fare, new.vkt)

# metro data ----
metro.gtfs.file <- 'data/gtfs/4.b metro.future.zip'
metro.gtfs <- read_gtfs(metro.gtfs.file) %>% 
  get_shapes()

metro.gtfs$stops$stop_id <- paste0('M', metro.gtfs$stops$stop_id)
metro.gtfs$stop_times$stop_id <- paste0('M', metro.gtfs$stop_times$stop_id)

# super gtfs -------------------------------------------------------------------------
bus.gtfs.file <- 'data/gtfs/5. bus.pre.optimal.zip'
bus.gtfs <- read_gtfs(bus.gtfs.file)

# optimization 35 -----------------------------------------------------------------------------
# pop.1 + ttm ----------------------------------------------------------------------------------------
trip.distances <- 
  get_distances(bus.gtfs, method = 'by.trip') %>% 
  select(route_id, trip_id, distance) %>% 
  arrange(-distance)

scenarios <- optimist.subsidy$id

first.initial.guess <- 
  trip.distances %>% 
  mutate(cum.distance = cumsum(distance) %>% as.numeric) %>%
  bind_cols(optimist.subsidy %>% 
              select(id, new.vkt) %>% 
              pivot_wider(names_from = id, values_from = new.vkt)) %>% 
  mutate_at(5:14, function(x){x >= .$cum.distance}) %>% 
  select(5:14) %>% 
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
  
  popSize <- 10
  elitism <- 5
  
  guessol <-
    ga(type = "binary",
       fitness = guessfit,
       nBits = nrow(trip.distances), 
       popSize = popSize,
       maxiter = 500,
       pmutation = .8,
       pcrossover = .8,
       elitism = elitism,
       suggestions = t(proposition),
       names = trip.distances$trip_id,
       budget.vkt = budget.vkt
    )
  
  return(guessol)
  
}

guessol <- 
  sapply(scenarios, second.guess)

populations.1 <-
  lapply(1:10, function(x){guessol[[x]]@population}) %>% 
  do.call(rbind, .) %>% 
  as_tibble() %>% 
    mutate(.before = 1,
           id = rep(scenarios, each = 10))

trips <- trip.distances$trip_id

service.list <- function(x){trips[populations.1 %>% select(-id) %>% .[x, ] == 1]}

calendar <- 
  tibble(trip_id = lapply(1:nrow(populations.1), service.list),
         monday = 1 %>% as.integer(),
         tuesday = 1 %>% as.integer(),
         wednesday = 1 %>% as.integer(),
         thursday = 1 %>% as.integer(),
         friday = 1 %>% as.integer(),
         saturday = 1 %>% as.integer(),
         sunday = 1 %>% as.integer(),
         start_date = lubridate::as_datetime('2021-12-12') + (86400 * 1:nrow(populations.1)), 
         end_date = start_date,
         service_id = paste0('scn', 1:nrow(populations.1))
  )

bus.gtfs$calendar <- calendar[, -1]

calendar.unnest <- 
  calendar[, c(1, 11)] %>% 
  unnest(trip_id) %>% 
  mutate(new_trip_id = 1:nrow(.) %>% as.character())

bus.gtfs$trips <-
  calendar.unnest %>% 
  left_join(bus.gtfs$trips %>% 
              select(-service_id)) %>% 
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

GTFSwizard::write_gtfs(bus.gtfs, 'data/r5rcore/gtfs.bus.zip')
beepr::beep()

r5rcore <-
  setup_r5('data/r5rcore', overwrite = TRUE)

departures <-
  ymd_hms(unique(calendar$start_date) + 23400)

ttm_func <- 
  function(x) {
    
    travel_time_matrix(
      r5r_core = r5rcore,
      origins = origins,
      destinations = destinations,
      progress = FALSE,
      mode = 'TRANSIT',
      departure_datetime = x,
      time_window = 60,
      percentiles = 50, # decidir com moraes e justificar
      max_walk_time = 15,
      max_trip_duration = 120,
      n_threads = 7,
      draws_per_minute = 1) %>% 
      tibble %>% 
      setNames(c('from_id', 'to_id', 'travel_time')) %>% 
      data.table::fwrite(paste0("data/performance/ttm/35/it-1/", janitor::make_clean_names(x), '.csv'))
    
    gc()
    
  }

lapply(departures, ttm_func)

r5r::stop_r5()


# pop.1 accessibilidade -----------------------------------------------------------------------
critical.access <- read_lines('data/critical.access.txt') %>% as.numeric()

ttm.1.files <- 
  bind_rows(optimist.subsidy %>% select(-new.vkt),
          optimist.subsidy %>% select(-new.vkt),
          optimist.subsidy %>% select(-new.vkt),
          optimist.subsidy %>% select(-new.vkt),
          optimist.subsidy %>% select(-new.vkt),
          optimist.subsidy %>% select(-new.vkt),
          optimist.subsidy %>% select(-new.vkt),
          optimist.subsidy %>% select(-new.vkt),
          optimist.subsidy %>% select(-new.vkt),
          optimist.subsidy %>% select(-new.vkt)) %>% 
  arrange(id) %>% 
  bind_cols(ttm.file = list.files('data/performance/ttm/fare optimal/35/it-1', '.csv', full.names = T))

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

i <- 1
index2access <- function(i) {
  
  ttm <- fread(ttm.1.files$ttm.file[i])
  
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
  
  cost.BR <- 20 * ttm.1.files$fare[i] * 2
  
  accessibility <- 
    populations %>% 
    left_join(cum.jobs) %>% 
    mutate(cum.jobs = if_else(is.na(cum.jobs), 0, cum.jobs),
           Rend_pc = Rend_pc * min.wage,
           transport.budget = if_else(work.class == 'formal' | age >= 60, 1,
                                      if_else(student == 'student', i.budget*Rend_pc*3/cost,
                                              if_else(income.class == 'BR', i.budget*Rend_pc/cost.BR, i.budget*Rend_pc/cost))),
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

tic <- Sys.time()
tot.problem.it1 <- lapply(1:100, index2access)
tac <- Sys.time()
tac - tic
beepr::beep()

saveRDS(tot.problem.it1, 'data/performance/ttm/35/it-1/totproblem.rds')

tot.problem.it1 <- read_rds('data/performance/ttm/fare optimal/35/it-1/totproblem.rds')

ttm.1.files %>% 
  bind_cols(problem = unlist(tot.problem.it1)) %>% 
  arrange(problem)

unlist(tot.problem.it1) %>% sum
# optimization 60 ------------------------------------------------------------------------------------
# pop.2 + ttm ----------------------------------------------------------------------------------------
trip.distances <- 
  get_distances(bus.gtfs, method = 'by.trip') %>% 
  select(route_id, trip_id, distance) %>% 
  arrange(-distance)

optimist.subsidy <- 
  fread('data/subsidy.scenarios.it-2.csv') %>% 
  mutate(id = paste0('f', fare, 's', subsidy)) %>% 
  select(id, fare, new.vkt)

scenarios <- optimist.subsidy$id

first.initial.guess <- 
  trip.distances %>% 
  mutate(cum.distance = cumsum(distance) %>% as.numeric) %>%
  bind_cols(optimist.subsidy %>% 
              select(id, new.vkt) %>% 
              pivot_wider(names_from = id, values_from = new.vkt)) %>% 
  mutate_at(5:9, function(x){x >= .$cum.distance}) %>% 
  select(5:9) %>% 
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
    
    popSize <- 6
    elitism <- 3
    
    guessol <-
      ga(type = "binary",
         fitness = guessfit,
         nBits = nrow(trip.distances), 
         popSize = popSize,
         maxiter = 2500,
         pmutation = .8,
         pcrossover = .8,
         elitism = elitism,
         suggestions = t(proposition),
         names = trip.distances$trip_id,
         budget.vkt = budget.vkt
      )
    
    return(guessol)
    
  }

guessol <- 
  sapply(scenarios, second.guess)

populations.2 <-
  lapply(1:5, function(x){guessol[[x]]@population}) %>% 
  do.call(rbind, .) %>% 
  as_tibble() %>% 
  mutate(.before = 1,
         id = rep(scenarios, each = 6))

trips <- trip.distances$trip_id

service.list <- function(x){trips[populations.1 %>% select(-id) %>% .[x, ] == 1]}

calendar <- 
  tibble(trip_id = lapply(1:nrow(populations.2), service.list),
         monday = 1 %>% as.integer(),
         tuesday = 1 %>% as.integer(),
         wednesday = 1 %>% as.integer(),
         thursday = 1 %>% as.integer(),
         friday = 1 %>% as.integer(),
         saturday = 1 %>% as.integer(),
         sunday = 1 %>% as.integer(),
         start_date = lubridate::as_datetime('2021-12-12') + (86400 * 1:nrow(populations.2)), 
         end_date = start_date,
         service_id = paste0('scn', 1:nrow(populations.2))
  )

bus.gtfs$calendar <- calendar[, -1]

calendar.unnest <- 
  calendar[, c(1, 11)] %>% 
  unnest(trip_id) %>% 
  mutate(new_trip_id = 1:nrow(.) %>% as.character())

bus.gtfs$trips <-
  calendar.unnest %>% 
  left_join(bus.gtfs$trips %>% 
              select(-service_id)) %>% 
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

GTFSwizard::write_gtfs(bus.gtfs, 'data/r5rcore/gtfs.bus.zip')

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
      data.table::fwrite(paste0("data/performance/ttm/60/it-2/", janitor::make_clean_names(x), '.csv'))
    
    gc()
    
  }

lapply(departures, ttm_func)

r5r::stop_r5()


# pop.2 accessibilidade -----------------------------------------------------------------------
critical.access <- read_lines('data/critical.access.txt') %>% as.numeric()

ttm.2.files <- 
  bind_rows(optimist.subsidy %>% select(-new.vkt),
            optimist.subsidy %>% select(-new.vkt),
            optimist.subsidy %>% select(-new.vkt),
            optimist.subsidy %>% select(-new.vkt),
            optimist.subsidy %>% select(-new.vkt),
            optimist.subsidy %>% select(-new.vkt)) %>% 
  arrange(id) %>% 
  bind_cols(ttm.file = list.files('data/performance/ttm/60/it-2', '.csv', full.names = T))

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

#i <- 7
index2access <- function(i) {
  
  ttm <- fread(ttm.2.files$ttm.file[i])
  
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
  
  cost.BR <- 20 * ttm.2.files$fare[i] * 2
  
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

tic <- Sys.time()
tot.problem.it2 <- lapply(1:30, index2access)
tac <- Sys.time()
tac - tic
beepr::beep()

saveRDS(tot.problem.it2, 'data/performance/ttm/60/it-2/totproblem.rds')

tot.problem.it2 <- read_rds('data/performance/ttm/60/it-2/totproblem.rds')

pop1and2 <- 
  ttm.1.files %>% 
  bind_cols(problem = unlist(tot.problem.it1)) %>% 
  arrange(problem) %>% 
  left_join(fread('data/subsidy.scenarios.csv') %>% 
  filter(subsidy == .35) %>% 
  mutate(id = paste0( LETTERS[1:n()], '.', subsidy) %>% 
           str_remove('0.'),
         new.id = paste0('f', fare, 's', subsidy)) %>% 
  select(id, new.id),
  by = 'id') %>% 
  select(id = new.id, fare, problem) %>% 
  bind_rows(ttm.2.files %>% 
              bind_cols(problem = unlist(tot.problem.it2)) %>% 
              select(id, fare, problem)) %>% 
  mutate(subsidy = rep(c(.35, .6), times = c(100, 30)))

pop1and2 %>% 
  group_by(fare, subsidy) %>% 
  reframe(problem = mean(problem)) %>% 
  ggplot() +
  geom_line(aes(x = 4.5 - fare, y = problem, color = as.factor(subsidy), group = subsidy)) +
  geom_abline(slope = .5) +
  labs(x = 'discount over fare for low-income',y = 'PROBLEM', color = 'subsidy') +theme_minimal()

pop1and2 %>% arrange(problem) %>% print(100)

performance <- 
  lapply(list.files(path = 'data/performance', 'performance.csv', full.names = TRUE),
         data.table::fread) %>% 
  data.table::rbindlist()



# optimization fare $0 -------------------------------------------------------------------------------
# pop 3 + ttm ----------------------------------------------------------------------------------------
trip.distances <- 
  get_distances(bus.gtfs, method = 'by.trip') %>% 
  select(route_id, trip_id, distance) %>% 
  arrange(-distance)

optimist.subsidy <- 
  fread('data/subsidy.scenarios.it-3.csv') %>% 
  mutate(id = paste0('f', fare, 's', subsidy)) %>% 
  select(id, fare, new.vkt)

scenarios <- optimist.subsidy$id

first.initial.guess <- 
  trip.distances %>% 
  mutate(cum.distance = cumsum(distance) %>% as.numeric) %>%
  bind_cols(optimist.subsidy %>% 
              select(id, new.vkt) %>% 
              pivot_wider(names_from = id, values_from = new.vkt)) %>% 
  mutate_at(5:10, function(x){x >= .$cum.distance}) %>% 
  select(5:10) %>% 
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
    
    popSize <- 6
    elitism <- 3
    
    guessol <-
      ga(type = "binary",
         fitness = guessfit,
         nBits = nrow(trip.distances), 
         popSize = popSize,
         maxiter = 2500,
         pmutation = .8,
         pcrossover = .8,
         elitism = elitism,
         suggestions = t(proposition),
         names = trip.distances$trip_id,
         budget.vkt = budget.vkt
      )
    
    return(guessol)
    
  }

guessol <- 
  sapply(scenarios, second.guess)

populations.3 <-
  lapply(1:6, function(x){guessol[[x]]@population}) %>% 
  do.call(rbind, .) %>% 
  as_tibble() %>% 
  mutate(.before = 1,
         id = rep(scenarios, each = 6))

trips <- trip.distances$trip_id

service.list <- function(x){trips[populations.1 %>% select(-id) %>% .[x, ] == 1]}

calendar <- 
  tibble(trip_id = lapply(1:nrow(populations.3), service.list),
         monday = 1 %>% as.integer(),
         tuesday = 1 %>% as.integer(),
         wednesday = 1 %>% as.integer(),
         thursday = 1 %>% as.integer(),
         friday = 1 %>% as.integer(),
         saturday = 1 %>% as.integer(),
         sunday = 1 %>% as.integer(),
         start_date = lubridate::as_datetime('2021-12-12') + (86400 * 1:nrow(populations.3)), 
         end_date = start_date,
         service_id = paste0('scn', 1:nrow(populations.3))
  )

bus.gtfs$calendar <- calendar[, -1]

calendar.unnest <- 
  calendar[, c(1, 11)] %>% 
  unnest(trip_id) %>% 
  mutate(new_trip_id = 1:nrow(.) %>% as.character())

bus.gtfs$trips <-
  calendar.unnest %>% 
  left_join(bus.gtfs$trips %>% 
              select(-service_id)) %>% 
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

GTFSwizard::write_gtfs(bus.gtfs, 'data/r5rcore/gtfs.bus.zip')

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
      time_window = 60,
      percentiles = 50, # decidir com moraes e justificar
      max_walk_time = 15,
      max_trip_duration = 120,
      n_threads = 7,
      draws_per_minute = 1) %>% 
      tibble %>% 
      setNames(c('from_id', 'to_id', 'travel_time')) %>% 
      data.table::fwrite(paste0("data/performance/ttm/fare0/it-3/", janitor::make_clean_names(x), '.csv'))
    
    gc()
    
  }

lapply(departures, ttm_func)

r5r::stop_r5()

# pop.3 accessibilidade -----------------------------------------------------------------------
critical.access <- read_lines('data/critical.access.txt') %>% as.numeric()

ttm.3.files <- 
  bind_rows(optimist.subsidy %>% select(-new.vkt),
            optimist.subsidy %>% select(-new.vkt),
            optimist.subsidy %>% select(-new.vkt),
            optimist.subsidy %>% select(-new.vkt),
            optimist.subsidy %>% select(-new.vkt),
            optimist.subsidy %>% select(-new.vkt)) %>% 
  arrange(id) %>% 
  bind_cols(ttm.file = list.files('data/performance/ttm/fare0/it-3', '.csv', full.names = T))

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

#i <- 7
index2access <- function(i) {
  
  ttm <- fread(ttm.3.files$ttm.file[i])
  
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
  
  cost.BR <- 20 * ttm.3.files$fare[i] * 2
  
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

tic <- Sys.time()
tot.problem.it3 <- lapply(1:36, index2access)
tac <- Sys.time()
tac - tic
beepr::beep()

saveRDS(tot.problem.it3, 'data/performance/ttm/fare0/it-3/totproblem.rds')

tot.problem.it2 <- read_rds('data/performance/ttm/60/it-2/totproblem.rds')

var.data <- 
  ttm.2.files %>% 
  bind_cols(problem = unlist(tot.problem.it2)) %>% 
  select(id, fare, problem) %>% 
  mutate(subsidy = rep(.6, times = 30)) %>% 
  bind_rows(
    ttm.3.files %>%
      mutate(n = 1:36) %>% 
      arrange(-n) %>% 
      select(-n) %>% 
      bind_cols(problem = unlist(tot.problem.it3)) %>% 
      mutate(subsidy = rep(seq(.5, 0, -.1), each = 6))
  ) %>% 
  bind_rows(
    ttm.1.files %>% 
      bind_cols(problem = unlist(tot.problem.it1)) %>% 
      arrange(problem) %>% 
      left_join(fread('data/subsidy.scenarios.csv') %>% 
                  filter(subsidy == .35) %>% 
                  mutate(id = paste0( LETTERS[1:n()], '.', subsidy) %>% 
                           str_remove('0.'),
                         new.id = paste0('f', fare, 's', subsidy)) %>% 
                  select(id, new.id),
                by = 'id') %>% 
      select(fare, problem) %>% 
      mutate(subsidy = .35)
  ) %>% 
  select(-id, -ttm.file) 

var.data %>% 
  fwrite('data/performance/it.1.to.3.csv')

var.data %>% 
  ggplot() +
  geom_point(aes(x = subsidy, y = fare, size = problem, color = problem))

var.data %>% 
  ggplot() +
  geom_point(aes(x = subsidy, y = problem, size = fare, color = as.factor(fare)))

var.data %>% 
  ggplot() +
  geom_point(aes(x = fare, y = problem, size = subsidy, color = subsidy))


var.data %>% 
  ggplot +
  geom_tile(aes(x = fare, y = subsidy, fill = problem)) +
  viridis::scale_fill_viridis(option = 'H') +
  theme_minimal()

lm(data = var.data, formula = problem ~ fare + subsidy) %>% 
  summary

lm(data = var.data, formula = problem ~ fare + subsidy) %>% 
  plot

pop1and3 <- 
  ttm.3.files %>%
    mutate(n = 1:36) %>% 
    arrange(-n) %>% 
    select(-n) %>% 
    bind_cols(problem = unlist(tot.problem.it3)) %>% 
    mutate(subsidy = rep(seq(.5, 0, -.1), each = 6)) %>% 
    group_by(subsidy) %>% 
    reframe(problem = mean(problem)) %>% 
    ggplot +
    geom_line(aes(x = subsidy, y = problem))

performance <- 
  lapply(list.files(path = 'data/performance', 'performance.csv', full.names = TRUE),
         data.table::fread) %>% 
  data.table::rbindlist()
