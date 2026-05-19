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
options(java.parameters = '-Xmx7G')
library(r5r)

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
bus.files <- c('data/gtfs/1.b bus.baseline.zip', 'data/gtfs/6. bus.optimal.zip', 'data/gtfs/6. bus.optimal - maximin.zip')

metro.gtfs.file <- 'data/gtfs/4.b metro.future.zip'
metro.gtfs <- read_gtfs(metro.gtfs.file)

metro.gtfs$stops$stop_id <- paste0('M', metro.gtfs$stops$stop_id)
metro.gtfs$stop_times$stop_id <- paste0('M', metro.gtfs$stop_times$stop_id)

rm(metro.gtfs.file)

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

# TTM ----
bus.index.2ttm <- 
  function(i) {
  list.files('data/r5rcore', 'zip', full.names = T) %>% 
    unlink()
  
  stop_r5()
  write_gtfs(metro.gtfs, 'data/r5rcore/metro.gtfs.zip')
  read_gtfs(bus.files[i]) %>% 
    write_gtfs('data/r5rcore/bus.gtfs.zip')
  r5rcore <- setup_r5('data/r5rcore', overwrite = TRUE)
  
  ttm <-
    travel_time_matrix(
      r5r_core = r5rcore,
      origins = origins,
      destinations = destinations,
      progress = FALSE,
      mode = 'TRANSIT',
      departure_datetime = dmy_hms("13/12/2021 06:30:00"),
      time_window = 60,
      percentiles = 50, # decidir com moraes e justificar
      max_walk_time = 15,
      max_trip_duration = 120,
      n_threads = 7,
      draws_per_minute = 1) %>% 
    tibble %>% 
    setNames(c('from_id', 'to_id', 'travel_time'))
  
  return(ttm)

}

original.ttm <- 
  bus.index.2ttm(1)

optimal.ttm <- 
  bus.index.2ttm(2)

maximin.ttm <- 
  bus.index.2ttm(3)

ttm <- 
  original.ttm %>% 
  rename(original = travel_time) %>% 
  left_join(optimal.ttm %>% 
              rename(optimal = travel_time)) %>% 
  left_join(maximin.ttm %>% 
              rename(maximin = travel_time))

ttm %>% 
  fwrite('data/performance/ttm/all.ttm.csv')
# accessibility distribution -------------------------------------------------------------------
all.ttm <- fread('data/performance/ttm/all.ttm.csv')
min.wage <- 1518
cost <- 20 * 4.5 * 2
i.budget <- .1
cost.BR <- 0
sigma <- 50.95931 # 50% dos empregos alcançáveis em 60 minutos
U_i <- fread('data/performance/baseline.problem.csv') %>% select(id, U_i, p_i_before = p_i, accessibility_before = accessibility)
critical.access <- read_lines('data/critical.access.txt') %>% as.numeric()
donothing.access <- select(fread('data/performance/donothing.access.csv'), id, do.nothing = accessibility)
beepr::beep(4)

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

cum.jobs <- 
  left_join(od, all.ttm) %>% 
  mutate_at(3:5, function(x) if_else(is.na(x), Inf, x)) %>% 
  filter(!from_id == to_id) %>% 
  rename(hex = to_id) %>% 
  left_join(jobs %>% select(-scenario)) %>% 
  #na.omit() %>% 
  pivot_longer(3:5, names_to = 'scenario', values_to = 'travel_time') %>% 
  group_by(from_id, travel_time, job.class, scenario) %>% 
  reframe(jobs = sum(jobs)) %>%
  mutate(dec.jobs = jobs * rvmethod::gaussfunc(travel_time, 0, sigma)) %>%
  rename(hex = from_id) %>% 
  group_by(hex, job.class, scenario) %>% 
  reframe(cum.jobs = sum(dec.jobs)) %>% 
  mutate(cum.jobs = if_else(cum.jobs == 0, min(cum.jobs[cum.jobs != 0])/2, cum.jobs),
         income.class = if_else(job.class == "jobs.high", 'AR', if_else(job.class == 'jobs.med', 'MR', 'BR')))

accessibility <- 
  populations %>% 
  select(-scenario) %>% 
  left_join(cum.jobs, relationship = 'many-to-many') %>% 
  mutate(cum.jobs = if_else(is.na(cum.jobs), 0, cum.jobs),
         Rend_pc = Rend_pc * min.wage,
         transport.budget = if_else(work.class == 'formal' | age >= 60, 1,
                                    if_else(student == 'student', i.budget*Rend_pc*3/cost,
                                            if_else(income.class == 'BR' & scenario != 'original', 1, i.budget*Rend_pc/cost))),
         transport.budget = if_else(transport.budget > 1, 1, transport.budget)) %>% 
  left_join(tot.jobs) %>% 
  mutate(tot.jobs = if_else(is.na(tot.jobs), 0, tot.jobs),
         accessibility = (cum.jobs * transport.budget) / tot.jobs)

i <- is.na(accessibility$scenario)
accessibility <- 
  bind_rows(accessibility[!i, ],
          accessibility[i, ] %>% mutate(scenario = 'original'),
          accessibility[i, ] %>% mutate(scenario = 'optimal'),
          accessibility[i, ] %>% mutate(scenario = 'maximin')) %>% 
  select(-job.class, -cum.jobs, -transport.budget, -tot.jobs)

rm(i)
beep(4)

accessibility <- 
  accessibility %>% 
  pivot_wider(names_from = scenario, values_from = accessibility) %>% 
  unnest(cols = c('maximin', 'optimal', 'original')) %>% 
  left_join(donothing.access %>% distinct()) %>% 
  mutate(do.nothing = ifelse(is.na(do.nothing), original, do.nothing)) %>% 
  pivot_longer(8:11, names_to = 'scenario', values_to = "accessibility") %>% 
  mutate(critical = if_else(accessibility < critical.access, 'critical', 'non.critical') %>% factor(levels = c('critical', 'non.critical')),
         problematic = if_else(income.class == 'BR' & critical == 'critical', 'problematic', 'non.problematic') %>% factor(levels = c('problematic', 'non.problematic')))

problem <- 
  accessibility %>% 
  left_join(U_i) %>% 
  rename(accessibility_after = accessibility) %>% 
  mutate(p_i_after = problematic == 'problematic',
         problem_before = p_i_before/U_i,
         problem_after = p_i_after/U_i,
         delta_access = accessibility_after - accessibility_before,
         restriction1 = p_i_before & delta_access < 0,
         restriction2 = p_i_after > p_i_before) %>% 
  select(-critical, -id, -problematic) %>% 
  mutate(accessibility_after = if_else(scenario == 'optimal' & restriction1, accessibility_before, accessibility_after), #*
         accessibility_after = if_else(scenario == 'optimal' & restriction2, critical.access, accessibility_after)) %>%  #*
  mutate(critical = if_else(accessibility_after < critical.access, 'critical', 'non.critical') %>% factor(levels = c('critical', 'non.critical')),
         problematic = if_else(income.class == 'BR' & critical == 'critical', 'problematic', 'non.problematic') %>% factor(levels = c('problematic', 'non.problematic'))) %>% 
  mutate(p_i_after = problematic == 'problematic',
         problem_before = p_i_before/U_i,
         problem_after = p_i_after/U_i,
         delta_access = accessibility_after - accessibility_before,
         restriction1 = p_i_before & delta_access < 0,
         restriction2 = p_i_after > p_i_before)

beep(sound = 4)

# comparing impacts ------------------------------------------------------
problem <- 
  problem %>% 
  mutate(scenario = scenario %>% str_replace('do.nothing', 'Problem Evolution') %>% 
           str_replace('original', 'Do Nothing') %>% 
           str_replace('optimal', 'Equitable Optimal') %>% 
           str_replace('maximin', 'Alternative Optimal') %>% 
           factor(., levels = c('Problem Evolution', 'Do Nothing', 'Equitable Optimal', 'Alternative Optimal')))

w.pop <- sum(1/(filter(problem, income.class == 'BR' & scenario == 'Problem Evolution') %>% .$U_i))

base.problem.pop <- 
  filter(problem, income.class == 'BR' & scenario == 'Problem Evolution') %>% 
  .$p_i_before %>% 
  sum()

problem %>% 
  group_by(scenario) %>% 
  reframe(problem = sum(problem_after)/w.pop,
          n = n(),
          restriction1 = sum(restriction1),
          restriction2 = sum(restriction2),
          perc.restriction1 = restriction1 / base.problem.pop,
          perc.restriction2 = restriction2 / n) %>% 
  .[, c(1, 2, 6, 7)] %>% 
  xtable::xtable()

baseline.problem <- 
  fread('data/performance/baseline.problem.csv')

sum(baseline.problem$problem)/(sum(1/filter(baseline.problem, income.class == 'BR')$U_i)) # baseline problem

# equitable optimal gtfs maps -----------------------------------------------------------------
eq.op.gtfs <-
  bus.files[2] %>% 
  read_gtfs()

baseline.gtfs <-
  bus.files[1] %>% 
  read_gtfs()

maximin.gtfs <-
  bus.files[3] %>% 
  read_gtfs()

distances <- lapply(bus.files[2:3], function(x){read_gtfs(x) %>% get_distances(method = 'by.trip')})
lapply(distances, function(x){x$distance %>% sum(na.rm = T)})

fleet <- lapply(bus.files[2:3], function(x){read_gtfs(x) %>% get_fleet(method = 'peak')})
lapply(fleet, function(x){x$fleet %>% max()})

eq.op.gtfs %>% 
  filter_route('express') %>% 
  .$shapes %>% 
  get_shapes_sf() %>% 
  ggplot() +
  geom_sf(data = shp.bairro, color = NA) +
  #geom_sf(data = get_shapes_sf(eq.op.gtfs$shapes), color = 'gray') +
  geom_sf(aes(color = 'Express\nRoute')) +
  theme_gtfswizard +
  theme(legend.title = element_blank())
ggsave('figs/express.png', dpi = 320, width = 6, height = 4.5)

eq.op.gtfs %>% 
  filter_route('express') %>%
  .$stop_times %>% 
  filter(trip_id == trip_id[820]) %>% view

route_frequency <- 
  eq.op.gtfs$trips$route_id %>% 
  str_remove_all("\\.x|\\.y") %>% 
  table %>% 
  as_tibble() %>% 
  setNames(c('route_id', 'optimal1')) %>% 
    left_join(baseline.gtfs$trips$route_id %>% 
                str_remove_all("\\.x|\\.y") %>% 
                table %>% 
                as_tibble() %>% 
                setNames(c('route_id', 'baseline'))
    ) %>% 
    left_join(maximin.gtfs$trips$route_id %>% 
                str_remove_all("\\.x|\\.y") %>% 
                table %>% 
                as_tibble() %>% 
                setNames(c('route_id', 'optimal2'))
) %>% 
    mutate(diff.optimal1 = optimal1 - baseline,
           diff.optimal2 = optimal2 - baseline,
           perc.optimal1 = diff.optimal1 / baseline,
           perc.optimal2 = diff.optimal2 / baseline)
  
route_frequency

baseline.gtfs$trips %>% 
  select(route_id, shape_id) %>% 
  group_by(route_id) %>% 
  reframe(shape_id = shape_id[1]) %>% 
  left_join(baseline.gtfs %>% 
              get_shapes_sf() %>% 
              .$shapes) %>% 
  left_join(route_frequency) %>% 
  select(1, 4, 10, 11) %>% 
  pivot_longer(3:4, names_to = 'scenario', values_to = 'frequency_change') %>% 
  mutate(frequency_change = ifelse(is.na(frequency_change), 0, frequency_change)) %>% 
  filter(frequency_change != 0) %>% 
  mutate(scenario = scenario %>% str_replace('perc.optimal1', 'Equitable Optimal') %>% str_replace('perc.optimal2', 'Alternative Optimal')) %>% 
  st_as_sf() %>% 
  ggplot +
  geom_sf(data = shp.bairro, color = NA) +
  geom_sf(aes(color = frequency_change)) +
  #viridis::scale_color_viridis(option = 'H') +
  scale_color_gradient2(low = 'red', mid = 'gray', high = 'green', midpoint = 0, labels = scales::percent_format(), breaks = c(-.5, 0, .5)) +
  theme_linedraw() +
  facet_grid(.~scenario) +
  labs(color = 'Change in\nfrequency')
ggsave('figs/changefreq.png', dpi = 320, width = 12, height = 6)

baseline.gtfs$trips %>% 
  select(route_id, shape_id) %>% 
  group_by(route_id) %>% 
  reframe(shape_id = shape_id[1]) %>% 
  left_join(baseline.gtfs %>% 
  get_shapes_sf() %>% 
  .$shapes) %>% 
  left_join(route_frequency) %>% 
  mutate(diff.opt = perc.optimal1 - perc.optimal2) %>% 
  na.omit() %>% 
  filter(diff.opt < -.1 | diff.opt > .1) %>% 
  st_as_sf() %>% 
  ggplot +
  geom_sf(data = shp.bairro, color = NA) +
  geom_sf(aes(color = diff.opt), linewidth = 1) +
  #viridis::scale_color_viridis(option = 'H') +
  scale_color_gradient2(low = 'red', mid = 'gray', high = 'green', midpoint = 0, breaks = c(.4, -.25), labels = c('Routes more important\nto Equitable Optimal', 'Routes more important\nto Alternative Optimal')) +
  theme_gtfswizard +
  theme(legend.title = element_blank())
ggsave('figs/optimal.frequency.importance.png', dpi = 320, width = 7, height = 4.5)
  
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
  
# rascunho ------------------------------------------------------------------------------------
# comparing acessibility distribution ----
population <- sum(accessibility$pop)

perc.label <- 
  performance %>%
  mutate(people.critical.perc = (people.critical/population * 100) %>%
           round(., 1) %>%
           paste0(., '%\n', format(people.critical, big.mark = "."), ' ind.')) %>% 
  #select(scenario, people.critical.perc, max.bus.fleet, total.bus.distance) %>% 
  mutate(fleet.norm = max.bus.fleet / max(max.bus.fleet),
           distance.norm = total.bus.distance / max(total.bus.distance)) %>% 
  mutate(scenario = c('Baseline (a)', 'Do nothing (b)', 'Optimal (c)', 'Pasfor (d)'))

tot.jobs <- 
  filter(for.data, id_hex %in% destinations$id) %>% 
  mutate(trab = .$T002 + .$T003) %>% 
  .$trab %>% 
  sum()

accessibility %>% 
  pivot_longer(cols = c(2, 3, 4, 6), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing (b)') %>% str_replace('cum.jobs.opt', 'Optimal (c)') %>% str_replace('cum.jobs.pf', 'Pasfor (d)') %>% str_replace('cum.jobs.bl', 'Baseline (a)'),
         cum.jobs = cum.jobs/tot.jobs) %>% 
  ggplot +
  geom_histogram(aes(x = cum.jobs, weight = pop), bins = 60, fill = 'gray75') +
  geom_histogram(data = . %>% filter(cum.jobs < critical.access/tot.jobs), aes(x = cum.jobs, weight = pop, fill = 'Individuals\nin critical\naccessibility\nconditions'), bins = 60) +
  geom_vline(aes(color = paste0('Critical\naccessibility\nthershold\nof ', format(round(critical.access/tot.jobs*100, 1), big.mark = "."), '%'), xintercept = critical.access/tot.jobs), linetype = 'dashed', linewidth = 1) +
  geom_text(data = perc.label, aes(x = .38, y = 10000, label = people.critical.perc), color = 'white') +
  # geom_point(data = perc.label, aes(x = 2500, 40000, size = total.bus.distance/max(total.bus.distance)), color = 'blue') +
  # geom_text(data = perc.label, aes(x = 3500, 40000, label = paste0('Bus\nlength\n', format(round(total.bus.distance/1000), big.mark = '.'), 'km'))) +
  # geom_point(data = perc.label, aes(x = 12000, 40000, size = total.metro.distance/max(total.metro.distance)), color = 'green') +
  # geom_text(data = perc.label, aes(x = 13000, 40000, label = paste0('Metro\nlength\n', format(round(total.metro.distance/1000), big.mark = '.'), 'km'))) +
  facet_wrap(.~scenario, ncol = 2) +
  theme_gtfswizard +
  labs(title = 'Accessibility distribution', x = 'Accessibility (reachable jobs)', y = 'Low-income individuals from 19 to 69 yo', color = '', fill = '') +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  scale_x_percent(breaks = seq(0, .75, .15)) +
  scale_fill_manual(values = 'red') +
  scale_color_manual(values = 'black')
ggsave('figs/problem.distribution.png', width = 7.5, height = 5, scale = 1.45)

# accessibility critical threshold sensibility analisys ----
population <- sum(accessibility$pop)

thresholds <- 
  tibble(poverty.threshold = seq(0, 1, .05),
         reachable.jobs = poverty.threshold * tot.jobs)

accessibility %>% 
  select(-geom) %>% 
  pivot_longer(c(2:4, 6), names_to = 'scenario', values_to = 'reach.jobs') %>% 
  bind_cols(pivot_wider(thresholds, names_from = 1, values_from = 2)) %>% 
  pivot_longer(5:25, names_to = 'poverty.threshold', values_to = 'jobs.threshold') %>% 
  mutate(poor = if_else(reach.jobs - jobs.threshold > 0, 'not-access-poor', 'access-poor')) %>% 
  filter(poor == 'access-poor') %>% 
  group_by(scenario, poverty.threshold) %>%
  reframe(pop = sum(pop)/population) %>% 
  mutate(poverty.threshold = as.numeric(poverty.threshold),
         scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor') %>% str_replace('cum.jobs.bl', 'Baseline')) %>% 
  #arrange(pop)
  filter(poverty.threshold <= .75) %>% 
  ggplot() +
  #geom_line(aes(x = poverty.threshold, y = pop, color = scenario), linewidth = .8, alpha = .7) +
  geom_line(aes(x = poverty.threshold, y = pop, color = scenario)) +
  geom_point(aes(x = poverty.threshold, y = pop, color = scenario)) +
  theme_gtfswizard +
  scale_x_percent(breaks = seq(0, .75, .15)) +
  scale_y_percent() +
  theme(legend.title = element_blank()) +
  labs(x = 'Critical Accessibility Threshold (% jobs reachable)', y = 'Low-income individuals\nfrom 19 to 69 yo (%)', title = 'Critical Accessibility Threshold Sensibility Analisys')
ggsave('figs/cdf2.png', width = 15, dpi = 320, scale = .75)

# gini index ----
gini.dat <- 
  accessibility %>% 
  pivot_longer(cols = c(2, 3, 4, 6), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  select(-geom) %>% 
  mutate(tot.access = pop * cum.jobs,
         cum.jobs = cum.jobs/tot.jobs) %>% 
  arrange(scenario, cum.jobs) %>% 
  group_by(scenario) %>% 
  mutate(cum.tot.access = cumsum(tot.access), 
         cumpop = cumsum(pop)) %>%
  mutate(cum.tot.access = cum.tot.access/max(cum.tot.access), 
         cumpop = cumpop/max(cumpop)) %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor') %>% str_replace('cum.jobs.bl', 'Baseline'))
  

gini.dat %>% 
  ggplot() +
  geom_abline(aes(intercept = 0, slope = 1, linetype = 'Equality\nline')) +
  geom_vline(aes(xintercept = .5, linetype = 'Critical\naccessibility\nlevel')) +
  geom_line(aes(y = cum.tot.access, x = cumpop, color = scenario)) +
  theme_gtfswizard +
  scale_x_percent() +
  scale_y_percent() +
  scale_linetype_manual(values = c('dashed', 'solid')) +
  labs(title = 'Lorenz curve', y = 'Cumulative accessibility', x = 'Cumulative population', color = '', linetype = '') +
  theme(legend.position = 'bottom')
ggsave('figs/lorenz.png', scale = 1)

Ts <- (.5 * .5) / 2
Tns <- .5 * .75

gini.dat %>% 
  group_by(scenario, cum.tot.access) %>% 
  reframe(cumpop = cumpop[n()],
          critical = if_else(cumpop <= .5, 'Bs', 'Bns')) %>%
  arrange(scenario, cumpop) %>% 
  group_by(scenario, critical) %>% 
  mutate(dx = lead(cumpop) - cumpop,
         ybar = (lead(cum.tot.access) + cum.tot.access) / 2,
         area = dx * ybar) %>% 
  group_by(scenario, critical) %>% 
  reframe(area = sum(area, na.rm = T)) %>% 
  pivot_wider(names_from = 'critical', values_from = 'area') %>% 
  group_by(scenario) %>% 
  reframe(B = Bns + Bs,
         As = Ts - Bs,
         Asn = Tns - Bns,
         A = As + Asn,
         gini = A/(A+B),
         severity = As/(A+B),
         severity.perc = severity/gini)

# Quantas pessoas em cada decil de renda está um situação de problema? ----
accessibility %>% 
  mutate(cum.jobs.bl = if_else(cum.jobs.bl < critical.access, 'critical', 'not critical'),
         cum.jobs.dn = if_else(cum.jobs.dn < critical.access & cum.jobs.bl == 'critical', 'critical',
                               if_else(cum.jobs.dn < critical.access & cum.jobs.bl == 'not critical', 'new critical',
                                       if_else(cum.jobs.dn > critical.access & cum.jobs.bl == 'critical', 'new not critical',
                                               'not critical'))),
         cum.jobs.pf = if_else(cum.jobs.pf < critical.access & cum.jobs.bl == 'critical', 'critical',
                               if_else(cum.jobs.pf < critical.access & cum.jobs.bl == 'not critical', 'new critical',
                                       if_else(cum.jobs.pf > critical.access & cum.jobs.bl == 'critical', 'new not critical',
                                               'not critical'))),
         cum.jobs.opt = if_else(cum.jobs.opt < critical.access & cum.jobs.bl == 'critical', 'critical',
                                if_else(cum.jobs.opt < critical.access & cum.jobs.bl == 'not critical', 'new critical',
                                        if_else(cum.jobs.opt > critical.access & cum.jobs.bl == 'critical', 'new not critical',
                                                'not critical')))
  ) %>% 
  pivot_longer(cols = c(2, 3, 4, 6), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing (a)') %>% str_replace('cum.jobs.opt', 'Optimal (b)') %>% str_replace('cum.jobs.pf', 'Pasfor (c)')) %>% 
  left_join(for.data %>% 
              filter(id_hex %in% accessibility$id_hex) %>% 
              select(id_hex, R003)) %>% 
  mutate(cum.jobs = cum.jobs %>% str_remove('new ')) %>% 
  group_by(R003, scenario, cum.jobs) %>% 
  reframe(pop = sum(pop, na.rm = T)) %>% 
  pivot_wider(names_from = scenario, values_from = pop) %>% 
  mutate_at(3:5, function(x){x - .$cum.jobs.bl}) %>%
  pivot_longer(cols = c(3:5), names_to = 'scenario', values_to = 'diff.pop') %>% 
  filter(cum.jobs == 'critical') %>% 
  ggplot() +
  geom_col(aes(x = R003, y = diff.pop), fill = 'firebrick', position = 'stack') +
  geom_shadowtext(aes(x = R003, y = diff.pop/2, label = format(paste0(round(diff.pop/cum.jobs.bl*100, 1), '%'), vjust = -1))) +
  facet_grid(.~scenario) +
  theme_gtfswizard +
  scale_y_comma(big.mark = '.') +
  scale_x_continuous(breaks = 1:6) +
  labs(x = 'Income decile', fill = '', y = 'Difference in population', title = 'Difference in population under critical accessibility levels by income decile and scenario')
ggsave('figs/diff.individuals.problem.income.png', dpi = 320, scale = 1.5, height = 4)

accessibility %>% 
  mutate(cum.jobs.bl = if_else(cum.jobs.bl < critical.access, 'critical', 'not critical'),
         cum.jobs.dn = if_else(cum.jobs.dn < critical.access & cum.jobs.bl == 'critical', 'critical',
                               if_else(cum.jobs.dn < critical.access & cum.jobs.bl == 'not critical', 'new critical',
                                       if_else(cum.jobs.dn > critical.access & cum.jobs.bl == 'critical', 'new not critical',
                                               'not critical'))),
         cum.jobs.pf = if_else(cum.jobs.pf < critical.access & cum.jobs.bl == 'critical', 'critical',
                               if_else(cum.jobs.pf < critical.access & cum.jobs.bl == 'not critical', 'new critical',
                                       if_else(cum.jobs.pf > critical.access & cum.jobs.bl == 'critical', 'new not critical',
                                               'not critical'))),
         cum.jobs.opt = if_else(cum.jobs.opt < critical.access & cum.jobs.bl == 'critical', 'critical',
                                if_else(cum.jobs.opt < critical.access & cum.jobs.bl == 'not critical', 'new critical',
                                        if_else(cum.jobs.opt > critical.access & cum.jobs.bl == 'critical', 'new not critical',
                                                'not critical')))
  ) %>% 
  pivot_longer(cols = c(2, 3, 4, 6), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.bl', 'Baseline (a)') %>% str_replace('cum.jobs.dn', 'Do nothing (b)') %>% str_replace('cum.jobs.opt', 'Optimal (c)') %>% str_replace('cum.jobs.pf', 'Pasfor (d)')) %>% 
  left_join(for.data %>% 
              filter(id_hex %in% accessibility$id_hex) %>% 
              select(id_hex, R003)) %>% 
  mutate(cum.jobs = cum.jobs %>% str_remove('new ')) %>% 
  group_by(R003, scenario, cum.jobs) %>% 
  reframe(pop = sum(pop, na.rm = T)) %>% 
  group_by(R003, scenario) %>% 
  mutate(perc.pop = (pop / sum(pop) * 100) %>% 
           round(1) %>% paste0(., '%'),
         y = if_else(cum.jobs == 'not critical', pop/2, pop/2 + lead(pop))) %>% 
  ggplot() +
  geom_col(aes(x = R003, y = pop, fill = cum.jobs), position = 'stack') +
  geom_shadowtext(aes(x = R003, y = y, label = perc.pop)) +
  facet_grid(.~scenario) +
  theme_gtfswizard +
  scale_y_comma(big.mark = '.') +
  scale_x_continuous(breaks = 1:6) +
  theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('firebrick', 'green4')) +
  labs(x = 'Income decile', fill = '', y = 'Population', title = 'Population by accessibility levels, income decile and scenario')
ggsave('figs/individuals.problem.income.png', dpi = 320, scale = 1.75, height = 4)

# boxplot ----
for.data %>% 
  filter(id_hex %in% accessibility$id_hex) %>% 
  select(id_hex, R003) %>% 
  left_join(accessibility, .) %>%
  mutate(blcritical = if_else(cum.jobs.bl > critical.access, 'Critical (i)', 'Not critical (ii)'),
         dncritical = if_else(cum.jobs.dn > critical.access, 'Critical (i)', 'Not critical (ii)'),
         pfcritical = if_else(cum.jobs.pf > critical.access, 'Critical (i)', 'Not critical (ii)'),
         optcritical = if_else(cum.jobs.opt > critical.access, 'Critical (i)', 'Not critical (ii)')) %>% 
  mutate_at(2:4, function(x){x - .$cum.jobs.bl}) %>% 
  select(-geom) %>% 
  mutate(R003 = factor(R003, levels = 1:6)) %>% 
  pivot_longer(cols = c(2, 3, 4), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  mutate(critical = if_else(scenario == 'cum.jobs.opt', optcritical, 
                            if_else(scenario == 'cum.jobs.bl', blcritical,
                                    if_else(scenario == 'cum.jobs.dn', dncritical, pfcritical)))) %>% 
  mutate(cum.jobs = cum.jobs/tot.jobs) %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing (a)') %>% str_replace('cum.jobs.opt', 'Optimal (b)') %>% str_replace('cum.jobs.pf', 'Pasfor (c)')) %>% 
  ggplot() +
  geom_hline(aes(yintercept = 0), linetype = 'dashed') +
  geom_boxplot(aes(x = R003, y = cum.jobs, color = R003, group = R003, weight = pop), fill = NA, outliers = FALSE) +
  #geom_violin(aes(x = R003, y = cum.jobs, fill = R003, group = R003, weight = pop), color = NA, outliers = FALSE) +
  theme_gtfswizard +
  viridis::scale_color_viridis(option = 'H', discrete = T) +
  scale_y_percent() +
  labs(x = 'Income decile', y = 'Accessibility difference (reachable jobs)', color = 'Income\ndecile') +
  facet_wrap(critical~scenario, ncol = 3)
ggsave('figs/boxplot.png', width = 9, height = 6)

# impact vs. accessibility distribution on baseline ----
accessibility %>% 
  mutate_at(2:4, function(x){(x - .$cum.jobs.bl)/tot.jobs}) %>% 
  pivot_longer(cols = c(2, 3, 4), names_to = 'scenario', values_to = 'cum.jobs') %>%
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor'),
         cum.jobs.bl = cum.jobs.bl/tot.jobs) %>% # a zona mais beneficiada deve ser a que possui menores niveis de cessibilidade independente quantidade de pessoas, pois nao se pode aceitar a reducao da acessibilidade de individuos em prol do aumento médio da acessibilidade
  left_join(., select(for.data, id_hex, R003) %>% tibble() %>% select(-geom)) %>% 
  ggplot() +
  geom_point(aes(x = cum.jobs.bl, y = cum.jobs, color = as_factor(R003)), alpha = .5) +
  theme_gtfswizard +
  theme(legend.position = 'bottom') +
  geom_smooth(aes(x = cum.jobs.bl, y = cum.jobs, group = R003), color = 'black', method = 'lm') +
  facet_grid(scenario~R003) +
  scale_x_percent() +
  scale_y_percent(limits = c(-.1, .1)) +
  labs(x = 'Baseline Accessibility (reachable jobs)', y = 'Accessibility Difference (reachable jobs)', title = 'Project impact by scenario, baseline accessibility and income decile', subtitle = 'Income decile', color = 'Income\ndecile')
ggsave('figs/scatterplot.png', scale = 1.2, dpi = 600, height = 6)

# sigma sensibility analisys ----
#resultados
# sigma <- 50.95931 # 50% nos empregos alcançáveis em 60 minutos
# sigma <- 25.479655 # 50% nos empregos alcançáveis em 30 minutos
# sigma <- 12.739827 # 50% nos empregos alcançáveis em 15 minutos
#rvmethod::gaussfunc(105, 0, 89.17879)

access.from.sigma <- function(sigma) {
  
  ttm.baseline <- 
    read_csv('data/performance/ttm/ttm.baseline.csv')
  
  accessibility <- 
    left_join(od, ttm.baseline) %>% 
    mutate(travel_time = if_else(is.na(travel_time), Inf, travel_time)) %>% 
    filter(!from_id == to_id) %>% 
    rename(id_hex = to_id) %>% 
    left_join(for.data) %>% 
    group_by(from_id, travel_time) %>% 
    reframe(jobs = sum(T002) + sum(T003)) %>%
    mutate(dec.jobs = jobs * rvmethod::gaussfunc(travel_time, 0, sigma)) %>%
    rename(id_hex = from_id) %>% 
    left_join(for.data) %>% 
    mutate(pop = P013 + P014 + P015) %>% 
    group_by(id_hex, pop) %>% 
    reframe(cum.jobs.bl = sum(dec.jobs)) %>% 
    left_join(for.data %>% select(id_hex, geom))
  
  ttm.donothing <- 
    read_csv('data/performance/ttm/ttm.donothing.csv')
  
  accessibility <- 
    left_join(od, ttm.donothing) %>% 
    mutate(travel_time = if_else(is.na(travel_time), Inf, travel_time)) %>% 
    filter(!from_id == to_id) %>% 
    rename(id_hex = to_id) %>% 
    left_join(for.data) %>% 
    group_by(from_id, travel_time) %>% 
    reframe(jobs = sum(T002) + sum(T003)) %>%
    mutate(dec.jobs = jobs * rvmethod::gaussfunc(travel_time, 0, sigma)) %>%
    rename(id_hex = from_id) %>% 
    group_by(id_hex) %>% 
    reframe(cum.jobs.dn = sum(dec.jobs)) %>% 
    left_join(accessibility)
  
  ttm.pasfor <- 
    read_csv('data/performance/ttm/ttm.pasfor.csv')
  
  accessibility <- 
    left_join(od, ttm.pasfor) %>% 
    mutate(travel_time = if_else(is.na(travel_time), Inf, travel_time)) %>% 
    filter(!from_id == to_id) %>% 
    rename(id_hex = to_id) %>% 
    left_join(for.data) %>% 
    group_by(from_id, travel_time) %>% 
    reframe(jobs = sum(T002) + sum(T003)) %>%
    mutate(dec.jobs = jobs * rvmethod::gaussfunc(travel_time, 0, sigma)) %>%
    rename(id_hex = from_id) %>% 
    group_by(id_hex) %>% 
    reframe(cum.jobs.pf = sum(dec.jobs)) %>% 
    left_join(accessibility)
  
  ttm.optimal <-
    data.table::fread('data/performance/ttm/ttm.optimal.csv')
  
  accessibility <- 
    left_join(od, ttm.optimal) %>% 
    mutate(travel_time = if_else(is.na(travel_time), Inf, travel_time)) %>% 
    filter(!from_id == to_id) %>% 
    rename(id_hex = to_id) %>% 
    left_join(for.data) %>% 
    group_by(from_id, travel_time) %>% 
    reframe(jobs = sum(T002) + sum(T003)) %>%
    mutate(dec.jobs = jobs * rvmethod::gaussfunc(travel_time, 0, sigma)) %>%
    rename(id_hex = from_id) %>% 
    group_by(id_hex) %>% 
    reframe(cum.jobs.opt = sum(dec.jobs)) %>% 
    left_join(accessibility) %>% 
      mutate(sigma)
  
  return(accessibility)
}

sigmas <- c('min15' = 12.739827, 'min30' = 25.479655, 'min45' = 38.21948, 'min60' = 50.95931, 'min75' = 63.699135, 'min90' = 76.43896, 'min105' = 89.17879, 'min120' = 101.91862)

var.sigma <- lapply(sigmas, access.from.sigma)

# sigma scenarios
accessibility.sigma <-
  data.table::rbindlist(var.sigma) %>% 
  tibble %>% 
    mutate(sigma.scenario = if_else(round(sigma) == round(12.73983), 'min15', 
                              if_else(round(sigma) == round(25.47966), 'min30', 
                                      if_else(round(sigma) == round(38.21948), 'min45',
                                              if_else(round(sigma) == round(50.95931), 'min60', 
                                                      if_else(round(sigma) == round(63.69913), 'min75',
                                                              if_else(round(sigma) == round(76.43896), 'min90',
                                                                      if_else(round(sigma) == round(89.17879), 'min105', 'min120')
                                                              )
                                                      )
                                              )
                                      )
                              )
    ) %>% factor(., levels = c('min15', 'min30', 'min45', 'min60', 'min75', 'min90', 'min105', 'min120'))
    )

sigmas.critical.access <- 
  lapply(var.sigma, function(x) {
  rep(x$cum.jobs.bl, x$pop) %>%
    quantile(., .5)
} ) %>% 
  unlist %>% 
  data.frame %>% 
  rownames_to_column() %>% 
  mutate(rowname = str_remove(rowname, '.50%')) %>% 
  setNames(c('sigma.scenario', 'critical.access')) %>% 
  as_tibble()

population <- sum(accessibility$pop)
  
sigma.performance <- 
  accessibility.sigma %>% 
  select(-geom) %>% 
  left_join(sigmas.critical.access, by = 'sigma.scenario') %>% 
  pivot_longer(c(2:4, 6), names_to = 'scenario', values_to = 'accessibility') %>% 
  filter(accessibility <= critical.access) %>%
  group_by(scenario, sigma.scenario, critical.access) %>% 
  reframe(critical.pop = sum(pop),
          perc.critical.pop = critical.pop/population) %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor') %>% str_replace('cum.jobs.bl', 'Baseline'),
         sigma.scenario = factor(sigma.scenario, levels = c('min15', 'min30', 'min45', 'min60', 'min75', 'min90', 'min105', 'min120')))

# graficos
sigma.performance %>% 
  ggplot() +
  geom_line(aes(x = sigma.scenario, y = perc.critical.pop, color = scenario, group = scenario), linewidth = 1) +
  theme_gtfswizard +
  scale_y_percent(limits = c(.4, .6)) +
  labs(x = 'Sigma Scenario', y = 'Low-income individuals from\n19 to 69 yo under critical\naccessibility levels (%)', color = '', title = 'Problem magnitude to sigma variation') +
  theme(legend.position = 'bottom')
ggsave('figs/sigma.scenarios.line.png', height = 4.5)

accessibility.sigma %>% 
  pivot_longer(cols = c(2, 3, 4, 6), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor') %>% str_replace('cum.jobs.bl', 'Baseline'),
         cum.jobs = cum.jobs/tot.jobs) %>% 
  left_join(sigma.performance, by = 'sigma.scenario') %>% 
  ggplot +
  geom_histogram(aes(x = cum.jobs, weight = pop), bins = 60, fill = 'gray75') +
  geom_histogram(data = . %>% filter(cum.jobs < critical.access/tot.jobs), aes(x = cum.jobs, weight = pop, fill = 'Individuals\nin critical\naccessibility\nconditions'), bins = 60) +
  geom_vline(data = sigma.performance, aes(color = paste0('Median\naccessibility\nof ', format(round(critical.access/tot.jobs*100, 1), big.mark = "."), '%'), xintercept = critical.access/tot.jobs), linetype = 'dashed', linewidth = 1) +
  ggrepel::geom_text_repel(data = sigma.performance, aes(x = critical.access/tot.jobs, y = critical.pop * 2, label = paste0(round(perc.critical.pop*100, 1), '%\n', format(critical.pop, big.mark = '.'))), color = 'black') +
  facet_grid(scenario~sigma.scenario, scales = 'free') +
  theme_gtfswizard +
  theme(legend.title = element_blank(), legend.position = 'bottom') +
  labs(x = 'Accessibility (reachable)', y = 'Low-income individuals from 19 to 69 yo', title = 'Problem distribution to sigma variation') +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  scale_x_percent() +
  scale_fill_manual(values = 'red')
ggsave('figs/sigma.hist.free.png', scale = 2)


# baseline access distribution ----------------------------------------------------------------
for.data %>% colnames
for.data %>% 
  tibble() %>% 
  select(id_hex, R003) %>% 
  na.omit() %>% 
  left_join(accessibility, .) %>% 
  mutate(cum.jobs.bl = cum.jobs.bl / tot.jobs) %>% 
  ggplot +
  geom_boxplot(aes(x = R003, cum.jobs.bl, group = R003, color = as_factor(R003)), fill = 'white') +
  geom_hline(aes(yintercept = mean(cum.jobs.bl), linetype = 'Mean\naccessibility')) +
  #geom_density(aes(cum.jobs.bl, color = as_factor(R003), group = R003), position = "stack", alpha = .2) +
  theme_gtfswizard +
  scale_y_percent() +
  scale_linetype_manual(values = 'dashed') +
  labs(title = 'Accessibility distribution by income decile', color = 'Income\ndecile', linetype = '', x = 'Income Decile', y = 'Basline accessibilitty')
ggsave('figs/diag1.png', width = 15, scale = .75, dpi = 320)

for.data %>% 
  tibble() %>% 
  select(id_hex, R001) %>% 
  na.omit() %>% 
  left_join(accessibility, .) %>% 
  mutate(cum.jobs.bl = cum.jobs.bl / tot.jobs) %>% 
  ggplot +
  geom_point(aes(x = R001, cum.jobs.bl), size = .5, color = 'gray70') +
  geom_smooth(aes(x = R001, cum.jobs.bl, color = 'Regression\nline'), method = 'lm') +
  #geom_hline(aes(yintercept = mean(cum.jobs.bl), linetype = 'Mean\naccessibility'), size = 1.5) +
  #geom_density(aes(cum.jobs.bl, color = as_factor(R003), group = R003), position = "stack", alpha = .2) +
  theme_gtfswizard +
  scale_y_percent() +
  scale_linetype_manual(values = 'dashed') +
  labs(title = 'Accessibility by income', color = '', linetype = '', x = 'Average household income per capita (R$)', y = 'Basline accessibilitty')
ggsave('figs/diag2.png', width = 15, scale = .75, dpi = 320)

for.data %>% 
  tibble() %>% 
  select(id_hex, R001) %>% 
  na.omit() %>% 
  left_join(accessibility, .) %>% 
  mutate(cum.jobs.bl = cum.jobs.bl / tot.jobs) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(data = shp.bairro, fill = 'white') +
  geom_sf(aes(fill = R001), color = NA) +
  viridis::scale_fill_viridis(option = "H", direction = -1) +
  theme_gtfswizard +
  labs(fill = 'Average\nhousehold\nincome per\ncapita (R$)', title = 'Income spatial distribution (a)')
ggsave('figs/diagmap1.png', dpi = 320, scale = .75)

for.data %>% 
  tibble() %>% 
  select(id_hex, R001) %>% 
  na.omit() %>% 
  left_join(accessibility, .) %>% 
  mutate(cum.jobs.bl = cum.jobs.bl / tot.jobs) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(data = shp.bairro, fill = 'white') +
  geom_sf(aes(fill = cum.jobs.bl), color = NA) +
  viridis::scale_fill_viridis(option = "H", direction = -1, labels = c('0%', '20%', '40%', '60%'), breaks = seq(0, .6, .2)) +
  theme_gtfswizard +
  labs(fill = 'Accessibility', title = 'Accesbility spatial distribution (b)')
ggsave('figs/diagmap2.png', dpi = 320, scale = .75)

for.data %>% 
  tibble() %>% 
  select(id_hex, R001) %>% 
  na.omit() %>% 
  left_join(accessibility, .) %>% 
  mutate(cum.jobs.bl = cum.jobs.bl / tot.jobs) %>% 
  .$cum.jobs.bl

cor.test(for.data %>% 
           tibble() %>% 
           select(id_hex, R001) %>% 
           na.omit() %>% 
           left_join(accessibility, .) %>% 
           mutate(cum.jobs.bl = cum.jobs.bl / tot.jobs) %>% 
           .$R001,
         for.data %>% 
           tibble() %>% 
           select(id_hex, R001) %>% 
           na.omit() %>% 
           left_join(accessibility, .) %>% 
           mutate(cum.jobs.bl = cum.jobs.bl / tot.jobs) %>% 
           .$cum.jobs.bl)