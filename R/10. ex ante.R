# Universidade Federal do Ceará
# Programa de Pós-Graduação em Engenharia de Transportes
# Nelson de O. Quesado Filho
# Julho de 2024
rm(list = ls()); gc()

# preambulo ----
library(tidyverse)
library(tidylog)
library(data.table)
library(shadowtext)
library(beepr)
library(sf)

library(GTFSwizard)
library(aopdata)
#options(java.parameters = '-Xmx7G')
#library(r5r)

theme_gtfswizard <-
  hrbrthemes::theme_ipsum(base_family = "Times New Roman",
                          axis_title_face = 'bold') +
  ggplot2::theme(title = ggplot2::element_text(size = 11, color = '#283c42'),,
                 axis.title.x = ggplot2::element_text(size = 11, color = '#283c42'),
                 axis.title.y = ggplot2::element_text(size = 11, color = '#283c42'),
                 axis.text.x = ggplot2::element_text(size = 10),
                 axis.text.y = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

# ts data ----
# metro.future.gtfs <- read_gtfs('data/gtfs/4.b metro.future.zip')
# metro.future.gtfs$stops$stop_id <- paste0('M', metro.future.gtfs$stops$stop_id)
# metro.future.gtfs$stop_times$stop_id <- paste0('M', metro.future.gtfs$stop_times$stop_id)
# 
# metro.baseline.gtfs <- read_gtfs('data/gtfs/3.b metro.baseline.zip')
# metro.baseline.gtfs$stops$stop_id <- paste0('M', metro.baseline.gtfs$stops$stop_id)
# metro.baseline.gtfs$stop_times$stop_id <- paste0('M', metro.baseline.gtfs$stop_times$stop_id)
# 
# metro.empty.gtfs <- metro.baseline.gtfs %>% filter_time(from = "06:20:00", to = '06:20:01')
# 
# bus.baseline.gtfs <- read_gtfs('data/gtfs/6. bus.optimal.zip')
# 
# routes.5 <- sample(bus.baseline.gtfs$routes$route_id, replace = F, size = round(length(bus.baseline.gtfs$routes$route_id)*.05))
# routes.15 <- sample(bus.baseline.gtfs$routes$route_id, replace = F, size = round(length(bus.baseline.gtfs$routes$route_id)*.15))
# 
# stops.5 <- sample(bus.baseline.gtfs$stops$stop_id, replace = F, size = round(length(bus.baseline.gtfs$stops$stop_id)*.95))
# stops.15 <- sample(bus.baseline.gtfs$stops$stop_id, replace = F, size = round(length(bus.baseline.gtfs$stops$stop_id)*.85))
# 
# bus.5route.0stop.gtfs <- filter_route(bus.baseline.gtfs, routes.5, keep = FALSE)
# bus.15route.0stop.gtfs <- filter_route(bus.baseline.gtfs, routes.15, keep = FALSE)
# 
# bus.5route.5stop.gtfs <- filter_stop(bus.baseline.gtfs, stops.5) %>%  filter_route(routes.5, keep = FALSE)
# bus.15route.5stop.gtfs <- filter_stop(bus.baseline.gtfs, stops.5) %>%  filter_route(routes.15, keep = FALSE)
# 
# bus.5route.15stop.gtfs <- filter_stop(bus.baseline.gtfs, stops.15) %>%  filter_route(routes.5, keep = FALSE)
# bus.15route.15stop.gtfs <- filter_stop(bus.baseline.gtfs, stops.15) %>%  filter_route(routes.15, keep = FALSE)
# 
# bus.0route.5stop.gtfs <- filter_stop(bus.baseline.gtfs, stops.5)
# bus.0route.15stop.gtfs <- filter_stop(bus.baseline.gtfs, stops.15)

gtfs.scenbarios <- 
  tibble(bus = list(c('bus.baseline.gtfs', 'bus.5route.0stop.gtfs', 'bus.15route.0stop.gtfs', 'bus.5route.5stop.gtfs', 'bus.15route.5stop.gtfs', 'bus.5route.15stop.gtfs', 'bus.15route.15stop.gtfs', 'bus.0route.5stop.gtfs', 'bus.0route.15stop.gtfs')),
         metro = list(c('metro.baseline.gtfs', 'metro.future.gtfs', 'metro.empty.gtfs'))) %>% 
  unnest(cols = bus) %>% 
  unnest(cols = metro) %>% 
  mutate(gtfs.scenario.id = paste0('gs.', 1:27))


# lu data ----
# aop <- aopdata::read_grid('Fortaleza') %>% 
#   select(hex = id_hex) 

populations <-
  fread('data/landuse/populations.csv') %>% 
  filter(scenario != 'original') %>% 
  rename(ind.scenario = scenario) %>% 
  mutate_at(c(1, 3, 5:8), as_factor)

# hex.origin <-
#   populations %>%
#   filter(income.class == 'BR') %>%
#   .$hex %>%
#   unique
# 
# origins <-
#   read_grid(city = 'Fortaleza') %>%
#   filter(id_hex %in% hex.origin) %>%
#   select(id = id_hex) %>%
#   st_centroid()
# 
# jobs <-
#   fread('data/landuse/jobs.csv') %>% 
#   filter(jobs != 0) %>% 
#   rename(job.class = income.class,
#          job.scenario = scenario) 
# 
# tot.jobs <- 
#   jobs %>% 
#   group_by(job.class) %>% 
#   reframe(tot.jobs = sum(jobs, na.rm = T)) %>% 
#   mutate(income.class = c('AR', 'BR', 'MR')) %>% 
#   select(-job.class)
# 
# hex.destination <-
#   jobs %>%
#   .$hex %>%
#   unique
# 
# destinations <-
#   read_grid(city = 'Fortaleza') %>%
#   filter(id_hex %in% hex.destination) %>%
#   select(id = id_hex) %>%
#   st_centroid()
# 
# od <- 
#   tibble(from_id = origins$id) %>% 
#   group_by(from_id) %>% 
#   reframe(to_id = destinations$id) %>% 
#   filter(!from_id == to_id)
# 
# shp.bairro <-
#   read_sf('data/bairros', crs = 4326) %>%
#   select(geometry)

scenarios <- 
  gtfs.scenbarios %>% 
  mutate(fare = list(c(0, .5, 1.5))) %>% 
  unnest(cols = fare) %>% 
  mutate(job.scenario = list(c('pessimist', 'optimist', 'neutral'))) %>% 
  unnest(cols = job.scenario) %>% 
  mutate(ind.scenario = list(c('pessimist', 'optimist', 'neutral'))) %>% 
  unnest(cols = ind.scenario) %>% 
  mutate(scenario_id = paste0('scn', 1:nrow(.)), .before = bus)

# TTM ----
# ttm_func <- 
#   function(x) {
#   
#     list.files('data/r5rcore', 'zip', full.names = T) %>% unlink()
#     
#     gtfs.scenbarios[x, ]$bus %>% get %>% write_gtfs('data/r5rcore/bus.gtfs.zip')
#     gtfs.scenbarios[x, ]$metro %>% get %>% write_gtfs('data/r5rcore/metro.gtfs.zip')
#     
#     r5rcore <- setup_r5('data/r5rcore', overwrite = TRUE)
#     
#     travel_time_matrix(
#       r5r_core = r5rcore,
#       origins = origins,
#       destinations = destinations,
#       progress = T,
#       mode = 'TRANSIT',
#       departure_datetime = dmy_hms("13/12/2021 06:30:00"),
#       time_window = 60,
#       percentiles = 50, # decidir com moraes e justificar
#       max_walk_time = 15,
#       max_trip_duration = 120,
#       n_threads = 7,
#       draws_per_minute = 1) %>% 
#       tibble %>% 
#       setNames(c('from_id', 'to_id', 'travel_time')) %>% 
#       data.table::fwrite(paste0("data/performance/ttm/resilience/", janitor::make_clean_names(x), '.csv'))
#     
#     gc()
#     
#   }
# 
# for (x in 1:27) {ttm_func(x)}
# ttmfiles <- list.files('data/performance/ttm/resilience/', full.names = T)
# 
# list.ttm <- lapply(ttmfiles, fread)
# 
# all.ttm <- reduce(list.ttm, left_join, by = join_by(from_id, to_id))
# 
# all.ttm %>% 
#   setNames(c('from_id', 'to_id', paste0('gs.', 1:27))) %>% 
#   fwrite('data/performance/ttm/resilience/all.ttm.csv')

# all.ttm <- fread('data/performance/ttm/resilience/all.ttm.csv')
# accessibility distribution -------------------------------------------------------------------
# min.wage <- 1518
# cost <- 20 * 4.5 * 2
# i.budget <- .1
# sigma <- 50.95931 # 50% dos empregos alcançáveis em 60 minutos
# 
# access.func <- 
#   function(x) {
#   
#   gtfs.scenario.id <- scenarios[x, ]$gtfs.scenario.id
#   ttm <- all.ttm %>% select(from_id, to_id, all_of(gtfs.scenario.id))
#   
#   job.scn <- scenarios[x, ]$job.scenario
#   jobs2 <- jobs %>% filter(job.scenario == job.scn)
#   
#   ind.scn <- scenarios[x, ]$ind.scenario
#   populations2 <- populations %>% filter(ind.scenario == ind.scn)
#   
#   fare <- scenarios[x, ]$fare
#   
#   cost.br <- 20 * 4.5 * fare
#   
#   cum.jobs <- 
#     left_join(od, ttm) %>%
#     setNames(c('from_id', 'to_id', 'travel_time')) %>% 
#     mutate_at(3, function(x) if_else(is.na(x), Inf, x)) %>% 
#     filter(!from_id == to_id) %>% 
#     rename(hex = to_id) %>%
#     left_join(jobs2 %>% select(-job.scenario), relationship = 'many-to-many') %>%
#     na.omit() %>% 
#     group_by(from_id, travel_time, job.class) %>% 
#     reframe(jobs = sum(jobs, na.rm = T)) %>%
#     mutate(dec.jobs = jobs * rvmethod::gaussfunc(travel_time, 0, sigma)) %>%
#     rename(hex = from_id) %>% 
#     group_by(hex, job.class) %>%
#     reframe(cum.jobs = sum(dec.jobs, na.rm = T)) %>% 
#     mutate(cum.jobs = if_else(cum.jobs == 0, min(cum.jobs[cum.jobs != 0])/2, cum.jobs),
#            income.class = if_else(job.class == "jobs.high", 'AR', if_else(job.class == 'jobs.med', 'MR', 'BR')))
#   
#   accessibility <- 
#     populations2 %>% 
#     select(-ind.scenario) %>% 
#     left_join(cum.jobs, relationship = 'many-to-many') %>% 
#     mutate(cum.jobs = if_else(is.na(cum.jobs), 0, cum.jobs),
#            Rend_pc = Rend_pc * min.wage,
#            transport.budget = if_else(work.class == 'formal' | age >= 60, 1,
#                                       if_else(student == 'student', i.budget*Rend_pc*3/cost,
#                                               if_else(income.class == 'BR', i.budget*Rend_pc/cost.br, i.budget*Rend_pc/cost))),
#            transport.budget = if_else(transport.budget > 1, 1, transport.budget)) %>% 
#     left_join(tot.jobs) %>% 
#     mutate(tot.jobs = if_else(is.na(tot.jobs), 0, tot.jobs),
#            accessibility = (cum.jobs * transport.budget) / tot.jobs) %>% 
#     select(id, accessibility) %>% 
#     setNames(c('id', gtfs.scenario.id))
#   
#   scenario.id <- scenarios[x, ]$scenario_id
#   
#   #accessibility %>% fwrite(paste0("data/performance/ttm/resilience/access/", janitor::make_clean_names(scenario.id), '.csv'))
#   accessibility %>% saveRDS(paste0("data/performance/ttm/resilience/access/", janitor::make_clean_names(scenario.id), '.rds'))
#   
# }

# library(parallel)
# cl <- makeCluster(detectCores() - 1)
# clusterExport(cl, varlist = c("access.func", "scenarios", "all.ttm", "od", "jobs", "populations", "tot.jobs", "min.wage", "cost", "i.budget", "sigma"), envir = environment())
# clusterEvalQ(cl, {
#   library(tidyverse)
#   library(data.table)
#   library(sf)
#   library(dplyr)
#   library(rvmethod)
#   library(janitor)
# })
# parLapply(cl, 1:nrow(scenarios), access.func)
# stopCluster(cl)


#lapply(1:nrow(scenarios), access.func)
# lapply(c(1:nrow(scenarios))[1:nrow(scenarios) %in% c(list.files("data/performance/ttm/resilience/access/", '.csv') %>% str_extract('\\d+') %>% as.numeric())], access.func)
# beep(4)


# accessibilities -----------------------------------------------------------------------------
# access.rds <- list.files('data/performance/ttm/resilience/access', full.names = T)
# 
# for (i in 1:length(access.rds)) {
#   
#  distinct(read_rds(access.rds[i])) %>% 
#     saveRDS(access.rds[i])
#   
#   gc()
#   
#   print(paste('rodando', i))
#   
# }

# 
# all.access <- tibble(id = NA)
# i <- 1
# for (i in 1:length(access.rds)) {
#   
#   all.access <- 
#     all.access %>% 
#     right_join(read_rds(access.rds[i]), by = 'id')
#   
#   gc()
#   
#   print(paste('rodando', i))
#   
# }
# 
# all.access %>% 
#   saveRDS('data/performance/ttm/resilience/access.all.rds')
# beep(4)


# results -------------------------------------------------------------------------------------
all.access <- 
  read_rds('data/performance/ttm/resilience/access.all.rds')


x <- 1
#all.access[x,] %>% t %>% .[,1] %>% as.numeric() %>% mean(na.rm = T)
for (x in 3813:nrow(all.access)) {
  
  all.access[x, -1] %>%
    t %>%
    as.tibble %>%
    reframe(id = as.character(all.access[x, ]$id), mean = mean(V1), min = min(V1, na.rm = T), sd = sd(V1, na.rm = T)) %>% 
    write_rds(paste0('data/performance/ttm/resilience/individuals/', all.access[x, ]$id, '.rds'))
  
}

ind.rds.files <- list.files('data/performance/ttm/resilience/individuals/', full.names = T)

results <- 
  lapply(ind.rds.files, read_rds) %>% 
  rbindlist()

results %>% 
  saveRDS('data/performance/ttm/resilience/individuals/results.rds')

results <-  
  read_rds('data/performance/ttm/resilience/individuals/results.rds')


U_i <- fread('data/performance/baseline.problem.csv') %>% select(id, U_i, p_i_before = p_i, accessibility_before = accessibility)

critical.access <- read_lines('data/critical.access.txt') %>% as.numeric()

results.all <- left_join(results, U_i)

# comparing impacts ------------------------------------------------------
results.all %>% 
  mutate(diff = mean - accessibility_before,
         percent_rank = percent_rank(U_i),
         class = ifelse(percent_rank <= .4, 'worse.off', ifelse(percent_rank >= .9, 'better.off', NA))) %>% 
  na.omit() %>% 
  group_by(class) %>% 
  reframe(diff = mean(diff),
          mean = mean(mean),
          min = mean(min),
          sd = mean(sd)) %>% 
  pivot_longer(2:5) %>% 
  pivot_wider(names_from = class) %>% 
  mutate(palma = better.off/worse.off) %>% 
  xtable::xtable()
  
  

results.all %>% 
  mutate(p_i_before = if_else(p_i_before, 'Problem', 'Non-Problem')) %>% 
  ggplot +
  geom_boxplot(aes(x = ntile(U_i, 10), y = mean-accessibility_before, color = ntile(U_i, 10) %>% as_factor(), group = ntile(U_i, 10))) +
  facet_grid(.~p_i_before) +
  theme_linedraw() +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  hrbrthemes::scale_y_percent() +
  labs(color = 'Vulnerability\ndecile (u_i)', x = 'Vulnerability decile (u_i)', y = 'Average Accessibility Reduction (difference in % of reachable jobs)')
ggsave('figs/a.png', dpi = 320, width = 10, height = 5.5)

results.all %>% 
  mutate(p_i_before = if_else(p_i_before, 'Problem', 'Non-Problem')) %>% 
  ggplot +
  geom_boxplot(aes(x = ntile(U_i, 10), y = mean, color = ntile(U_i, 10) %>% as_factor(), group = ntile(U_i, 10))) +
  facet_grid(.~p_i_before) +
  theme_linedraw() +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  hrbrthemes::scale_y_percent() +
  labs(color = 'Vulnerability\ndecile (u_i)', x = 'Vulnerability decile (u_i)', y = 'Average Disrupted Accessibility (% of reachable jobs)')
ggsave('figs/boxplot.resilience.mean.png', dpi = 320, width = 10, height = 5.5)

results.all %>% 
  mutate(p_i_before = if_else(p_i_before, 'Problem', 'Non-Problem')) %>% 
  ggplot +
  geom_boxplot(aes(x = ntile(U_i, 10), y = min, color = ntile(U_i, 10) %>% as_factor(), group = ntile(U_i, 10))) +
  facet_grid(.~p_i_before) +
  theme_linedraw() +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  hrbrthemes::scale_y_percent() +
  labs(color = 'Vulnerability\ndecile (u_i)', x = 'Vulnerability decile (u_i)', y = 'Minimum Disrupted Accessibility (% of reachable jobs)')
ggsave('figs/boxplot.resilience.min.png', dpi = 320, width = 10, height = 5.5)


results.all %>% 
  mutate(p_i_before = if_else(p_i_before, 'Problem', 'Non-Problem')) %>% 
  ggplot +
  geom_boxplot(aes(x = ntile(U_i, 10), y = sd, color = ntile(U_i, 10) %>% as_factor(), group = ntile(U_i, 10))) +
  facet_grid(.~p_i_before) +
  theme_linedraw() +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  hrbrthemes::scale_y_percent() +
  labs(color = 'Vulnerability\ndecile (u_i)', x = 'Vulnerability decile (u_i)', y = 'Disrupted Accessibility Standard Deviation (% of reachable jobs)')
ggsave('figs/boxplot.resilience.sd.png', dpi = 320, width = 10, height = 5.5)


# distributional effects ----------------------------------------------------------------------
problem

p <-
  problem %>% 
  mutate(p_i_before = if_else(p_i_before, 'Problem', 'Non-Problem')) %>% 
  ggplot +
  geom_boxplot(aes(x = ntile(Rend_pc, 10), y = delta_access, color = ntile(Rend_pc, 10) %>% as_factor(), group = ntile(Rend_pc, 10))) +
  facet_grid(scenario~p_i_before) +
  theme_linedraw() +
  labs(color = 'Income\ndecile')
#ggsave(plot = p, 'figs/boxplot.renda.png')

beepr::beep(4)

p <-
  problem %>% 
  mutate(p_i_before = if_else(p_i_before, 'Problem', 'Non-Problem')) %>% 
  ggplot +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(x = ntile(U_i, 10), y = delta_access, color = ntile(U_i, 10) %>% as_factor(), group = ntile(U_i, 10))) +
  facet_grid(p_i_before~scenario) +
  theme_linedraw() +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  hrbrthemes::scale_y_percent() +
  labs(color = 'Vulnerability\ndecile (u_i)', x = 'Vulnerability decile (u_i)', y = 'Intervention Impact (difference in % of reachable jobs)')
ggsave(plot = p, 'figs/boxplot.vulnerability.png', dpi = 320, width = 10, height = 4.5)

beepr::beep(4)

p <-
  problem %>% 
  pivot_longer(c(8, 11)) %>% 
  mutate(name = ifelse(name != 'accessibility_after', 'Baseline Accessibility', 'Intervention Accessibility')) %>% 
  ggplot +
  geom_boxplot(aes(x = ntile(U_i, 10), y = value, color = ntile(U_i, 10) %>% as_factor(), group = ntile(U_i, 10))) +
  facet_grid(name~scenario) +
  theme_linedraw() +
  hrbrthemes::scale_y_percent() +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  labs(color = 'Vulnerability\ndecile (u_i)', x = 'Vulnerability decile (u_i)', y = 'Accessibility (% of reachable jobs)')
ggsave(plot = p, 'figs/boxplot.accessibility.png', dpi = 320, width = 10, height = 4.5)

beepr::beep(4)

p <-
  problem %>% 
  mutate(p_i_before = if_else(p_i_before, 'Problem', 'Non-Problem')) %>% 
  ggplot +
  geom_point(aes(x = U_i, y = delta_access, color = ntile(U_i, 10) %>% as_factor())) +
  facet_grid(p_i_before~scenario) +
  theme_linedraw() +
  labs(color = 'Vulnerability\ndecile')
ggsave(plot = p, 'figs/scatter.vulnerability.png', dpi = 320, width = 10, height = 4.5)

beepr::beep(4)

# palma ratio ---------------------------------------------------------------------------------
problem %>% 
  select(U_i, scenario, accessibility_after) %>% 
  group_by(scenario) %>% 
  #mutate(percentile = percent_rank(U_i),
  mutate(percentile = percent_rank(accessibility_after),
         group = if_else(percentile <= .4, 'worse-off', if_else(percentile >= .9, 'better-off', NA))) %>% 
  na.omit() %>% 
  group_by(scenario, group) %>% 
  reframe(accessibility = mean(accessibility_after)) %>% 
  pivot_wider(names_from = 'group', values_from = 'accessibility') %>% 
  mutate(palma = `better-off`/`worse-off`) %>% 
  xtable::xtable()
  