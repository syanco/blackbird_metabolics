# Get Stats!!! #

# LIBRARIES #
library(tidyverse)
library(mgcv)
library(tidymv)
library(lubridate)


# get total diff

# MEAN #

met_mean <- readRDS("out/tot_out_experienced_temp_mean.rds") %>% 
  filter(julian.bird < 271) %>% 
  mutate(strat = fct_relevel(strat, rev(levels(strat))))


fm_therm_mean <- gam(met ~ strat + s(julian.bird, by = strat) + s(ring, bs = "re"), 
                     data = met_mean)

preds_mean <- get_gam_predictions(fm_therm_mean, series = julian.bird,
                                  series_length = max(met_mean$julian.bird)-min(met_mean$julian.bird))

# 1 Watt = 1 Joule/sec

secs_per_day <- 60*60*24
joules_df <- preds_mean %>% 
  mutate(joules = met*secs_per_day)

(tot_joules <- joules_df %>% 
  group_by(strat) %>% 
  summarize(tot_energy = sum(joules))
)

mig_j <- tot_joules %>% filter(strat == "fm") %>% pull(tot_energy)
res_j <- tot_joules %>% filter(strat == "ws") %>% pull(tot_energy)

res_j-mig_j
res_j/mig_j
