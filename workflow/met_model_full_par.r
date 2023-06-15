# LIBRARIES #

library(NicheMapR)
library(tidyverse)
# library(mgcv)
library(lubridate)
library(doRNG)
library(future)
library(foreach)
library(doFuture)
library(dplyr)
library(glue)

# SOURCE #

source("src/funs/run_model.r")

# INITS #

# size and shape
AMASS <- 0.1  # mass (kg)
SHAPE <- 4 # ellipsoid
SHAPE_B <- 2  # ratio length/width
SHAPE_B_MAX <- 3  # maximum ratio of length to width/depth
SAMODE <- 1 # bird surface area allometry

# environment
QSOLR <- 0
SHADE <- 100 # set shade to 100% (shouldn't matter with QSOLR=0)

# feather properties - these are straight from vignette (but modify fcn defaults) 
DHAIRD <- 3e-05  # hair diameter, dorsal (m)
DHAIRV <- 3e-05  # hair diameter, ventral (m)
LHAIRD <- 0.0231  # hair length, dorsal (m)
LHAIRV <- 0.0227  # hair length, ventral (m)
ZFURD <- 0.0058  # fur depth, dorsal (m)
ZFURV <- 0.0056  # fur depth, ventral (m)
RHOD <- 8e+07  # hair density, dorsal (1/m2)
RHOV <- 8e+07  # hair density, ventral (1/m2)
REFLD <- 0.248  # fur reflectivity dorsal (fractional, 0-1)
REFLV <- 0.351  # fur reflectivity ventral (fractional, 0-1)

TC_MAX <- 45 # set mac body temp

# Behavior
PCTWET_INC <- 0

# Morp
ANDENS <- 730 # body denisty kg/m3
SUBQFAT <- 1 # subQ fat is present

# DATA #
# 
# Load Blackbird data
load("data/df_bb_meta_20.01.23_interpolated.rdata")

dat0 <- df_bb_meta %>%
  filter(complete.cases(.)) %>%  # exclude missing data
  as.data.frame()

# get transition days
(fall_dep <- dat0 %>% 
    group_by(ring) %>%
    filter(status == "fall-migration") %>% 
    summarize(start = min(julian_bird)) %>% 
    summarize(mean(start)) %>% 
    as.numeric() %>% 
    round(0)
)

(spring_dep <- dat0 %>% 
    group_by(ring) %>%
    filter(status == "spring-migration") %>% 
    summarize(stop = max(julian_bird)) %>% 
    summarize(mean(stop)) %>% 
    as.numeric() %>% 
    round(0)
)

# Add microclimate buffering scenarios

dat1 <- dat0 %>% 
  mutate(experienced_temp_mean_buffer1 = case_when(strat == "ws" & julian_bird > fall_dep & julian_bird < spring_dep ~ experienced_temp_mean + 1,
                                                   TRUE ~ experienced_temp_mean),
         experienced_temp_minimum_buffer1 = case_when(strat == "ws" & julian_bird > fall_dep & julian_bird < spring_dep ~ experienced_temp_minimum + 1,
                                                   TRUE ~ experienced_temp_minimum),
         experienced_temp_maximum_buffer1 = case_when(strat == "ws" & julian_bird > fall_dep & julian_bird < spring_dep ~ experienced_temp_maximum + 1,
                                                   TRUE ~ experienced_temp_maximum),
         experienced_temp_mean_buffer2 = case_when(strat == "ws" & julian_bird > fall_dep & julian_bird < spring_dep ~ experienced_temp_mean + 2,
                                                   TRUE ~ experienced_temp_mean),
         experienced_temp_minimum_buffer2 = case_when(strat == "ws" & julian_bird > fall_dep & julian_bird < spring_dep ~ experienced_temp_minimum + 2,
                                                      TRUE ~ experienced_temp_minimum),
         experienced_temp_maximum_buffer2 = case_when(strat == "ws" & julian_bird > fall_dep & julian_bird < spring_dep ~ experienced_temp_maximum + 2,
                                                      TRUE ~ experienced_temp_maximum)
         )

# #summarize to daily time interval
# night1 <- night %>% 
#   #TODO: this is splitting across nights, need to get a measure of julian night
#   #TODO: run at higher temporal resolution
#   group_by(logger.id, julian.bird) %>% 
#   summarize(hrt = mean(hrt, na.rm = T), 
#             temp = mean(temp, na.rm = T),
#             airtemp = mean(airtemp, na.rm = T))

# create vector of individuals over which to loop
inds <- as.character(unique(dat0$ring))
#inds <- inds[1:2]


scenarios <- c("breedingsite_temp", 
               "experienced_temp_minimum", "experienced_temp_mean", "experienced_temp_maximum", 
               "experienced_temp_minimum_buffer1", "experienced_temp_mean_buffer1", "experienced_temp_maximum_buffer1",
               "experienced_temp_minimum_buffer2", "experienced_temp_mean_buffer2", "experienced_temp_maximum_buffer2")
# i <- 3
# j <- 2
for(j in 1:length(scenarios)) {
  
  dat_rename <- dat0 %>% 
    mutate(airtemp = !!as.name(scenarios[j]))
  
  # Init cluster
  registerDoFuture()
  plan(multisession, workers = 10, gc = TRUE)
  
  # Loop through individuals, fitting the endo model to each
  out <- foreach(i = 1:length(inds), .errorhandling = "pass", .inorder = F) %dorng% {
    
    # extract individual from the data
    dat_ind <- dat_rename %>% 
      filter(ring == inds[i]) %>%
      mutate(dt = ymd_hms(date_time),
             time = hour(dt) + minute(dt)/60) %>% # add time stamp
      arrange(julian_bird, time) %>% # sort by time
      dplyr::select(ring, julian_bird, time, heartrate, bodytemp, airtemp, strat) 
    
    if(nrow(dat_ind) > 0){ # check that individual was found and has data
      runMod(dat = dat_ind) # run the model
    } else {
      NULL
    } # if nrow > 0
  } #i
  
  # close cluster
  plan(sequential)
  
  # gather results
  tot_out_df <- do.call("rbind", out) %>% 
    drop_na(heartrate, met)
  
  saveRDS(tot_out_df, file = glue("out/tot_out_{scenarios[j]}.rds"))
  
  } #j 

