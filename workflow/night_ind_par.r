# LIBRARIES #

library(NicheMapR)
library(tidyverse)
# library(mgcv)
library(lubridate)
library(doRNG)
library(future)
library(foreach)
library(doFuture)

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
load("data/df_bb_meta_27.01.23_interpolated.rdata")

night <- df_bb_meta %>% 
  filter(dayphase_conservative == "night") %>% 
  filter(status == "on-site") %>% 
  mutate(airtemp = breedingsite_temp)

# #summarize to daily time interval
# night1 <- night %>% 
#   #TODO: this is splitting across nights, need to get a measure of julian night
#   #TODO: run at higher temporal resolution
#   group_by(logger.id, julian.bird) %>% 
#   summarize(hrt = mean(hrt, na.rm = T), 
#             temp = mean(temp, na.rm = T),
#             airtemp = mean(airtemp, na.rm = T))

# create vector of individuals over which to loop
inds <- unique(night$ring)
#inds <- inds[1:2]

registerDoFuture()
plan(multisession, workers = 48, gc = TRUE)

# i <- 2
# Loop through individuals, fitting the endo model to each
out <- foreach(i = 1:length(inds), .errorhandling = "pass", .inorder = F) %dorng% {
  
  # extract individual from the data
  dat1 <- night %>% 
    filter(ring == inds[i]) %>%
    mutate(dt = ymd_hms(date_time),
           time = hour(dt) + minute(dt)/60) %>% # add time stamp
    arrange(julian_bird, time) %>% # sort by time
    select(ring, julian_bird, time, heartrate, bodytemp, airtemp) %>%
    filter(complete.cases(.)) # exclude missing data
  
  if(nrow(dat1) > 0){ # check that individual was found and has data
    runMod(dat = dat1)
  } else {
    NULL
  } # if nrow > 0
} #i

plan(sequential)

night_out_df <- do.call("rbind", out) %>% 
  drop_na(heartrate, met)

saveRDS(night_out_df, file = "out/night_out.rds")