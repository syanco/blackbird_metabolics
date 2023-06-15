# LIBRARIES #

library(NicheMapR)
library(tidyverse)
library(mgcv)
library(lubridate)

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

night <- df_bb_meta %>% 
  filter(dayphase_conservative == "night")

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

# ENODTHERM MODEL

# create empty list to store results
out <- list()

# i <- 1
# Loop through individuals, fitting the endo model to each
for(i in 1:length(inds)){
  
  # extract individual from the data
  dat1 <- night %>% 
    filter(ring == inds[i]) %>% 
    mutate(dt = ymd_hms(date_time),
           time = hour(dt) + minute(dt)/60) %>% # add time stamp
    arrange(julian_bird, time) %>% # sort by time
    select(ring, julian_bird, time, heartrate, bodytemp, experienced_temp_mean) %>%
    filter(complete.cases(.)) # exclude missing data
  
  if(nrow(dat1) > 0){ # check that individual was found and has data
    
    # Run endo therm model 
    endo <- lapply(1:nrow(dat1), function(x) {
      endoR(WRITE_INPUT = 1,
            TC = dat1$temp[x],
            TA = dat1$airtemp[x],
            TC_MAX = TC_MAX,
            QSOLR = QSOLR,
            AMASS = AMASS,
            SHAPE = SHAPE,
            SHAPE_B = SHAPE_B,
            SHAPE_B_MAX = SHAPE_B_MAX,
            SAMODE = SAMODE,
            
            # env
            SHADE = SHADE,
            
            # feather properties
            DHAIRD = DHAIRD,
            DHAIRV = DHAIRV,
            LHAIRD = LHAIRD,
            LHAIRV = LHAIRV,
            ZFURD = ZFURD,
            ZFURV = ZFURV, 
            RHOD = RHOD,
            RHOV = RHOV,
            REFLD = REFLD,
            REFLV = REFLV,
            
            # behavior
            PCTWET_INC = PCTWET_INC,
            
            # morph
            ANDENS = ANDENS,
            SUBQFAT = SUBQFAT,
            
            # add metabolic multiplier
            TIMACT = 1,
            
            THERMOREG = 1
      )
    })
    
    # OUPUT #
    # 
    # extract the output
    endo.out <- do.call("rbind", lapply(endo, data.frame))
    
    # heat balance
    enbal <- endo.out[, grep(pattern = "enbal", colnames(endo.out))]
    colnames(enbal) <- gsub(colnames(enbal), pattern = "enbal.", 
                            replacement = "")
    
    # create output df including observed data
    tmp <- data.frame(hrt = dat1$heartrate, 
                      met = enbal$QGEN,
                      airtemp = dat1$experienced_temp_mean,
                      temp = dat1$bodytemp,
                      julian.bird = dat1$julian_bird,
                      ring = dat1$ring
    )
    
    out[[i]] <- tmp
  } else {
    next
    # out[[i <- NULL]]
  } # if nrow > 0
} #i



night_out_df <- do.call("rbind", out) %>% 
  drop_na(heartrate, met)


# HEART RATE ~ MET 

#hr ~ met
fm.hr <-gam(hrt ~ s(julian_bird) + met + s(ring, bs = "re") + s(ring, met, bs = "re"), 
            data = night_out_df)
summary(fm.hr)

# met ~ hr
fm.m <-gam(met ~ s(julian_bird) + hrt + s(ring, bs = "re") + s(ring, hrt, bs = "re"), 
           data = night_out_df)
summary(fm.m)
plot(fm.m, all.terms =T, shift = coef(fm.m)[1])


fm.m2 <-gam(met ~ s(julian_bird) + s(hrt) + s(ring, bs = "re") + s(ring, hrt, bs = "re"),
           data = night_out_df)
summary(fm.m2)
plot(fm.m2)

fm.0 <-gam(met ~  s(hrt) + s(ring, bs = "re") + s(ring, hrt, bs = "re"),
           data = night_out_df)
summary(fm.0)
plot(fm.0)
# WRITE OUT

save.image(file = "out/night_reg.rdata")
