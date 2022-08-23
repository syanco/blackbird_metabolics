# LIBRARIES #

library(NicheMapR)
library(tidyverse)

# DATA #
# 
# Load Blackbird data
night <- read.csv("data/df_bb_thermo_watts_night.csv")

# process data
# 
# body temps
bbtemp <- night %>% 
  #create "day" variable that starts at beginning of tracking instead of Jan 1 
  # mutate(jplus = julian.regular-243,
  #        jminus = julian.regular+123,
  #        jcon = case_when(jplus > 0 ~ jplus,
  #                         jminus > 0 ~ jminus))%>% 
  # get daily means for each strategy
  group_by(julian.bird) %>% 
  summarise(mbt = mean(temp, na.rm = T)) 
#TODO: cutoff temps at end of good data - julian day: 10 april (DOY 90)

# extract vector for fm
temp <- bbtemp %>% 
  arrange(julian.bird) %>% 
  pull(mbt)


# Heart rate
heartrate1 <- night %>% 
  # #create tracking day variable
  # mutate(jplus = julian.regular-243,
  #        jminus = julian.regular+123,
  #        jcon = case_when(jplus > 0 ~ jplus,
  #                         jminus > 0 ~ jminus)) %>% 
  # get daily means for each strategy
  group_by(julian.bird) %>% 
  summarise(mhr = mean(hrt, na.rm = T)) %>% 
  ungroup()

# # MICROCLIMATE #
# 
# loc <- c(47.77, 08.99)
# Usrhyt <- 0.5
# maxshade <- 100
# micro <- micro_global(loc = loc, Usrhyt = Usrhyt, maxshade = maxshade, timeinterval = 365)
# 
# metout <- as.data.frame(micro$metout)  # unshaded above-ground conditions
# 
# temps <- metout %>% 
#   group_by(DOY) %>% 
#   summarize(t = mean(TALOC)) 
# 
# #have to dupliacte day 365 because bird data has leap year... but met doesn't
# TAws <- c(temps$t[244:365], temps$t[365], temps$t[1:156])
# 
# TAfm <- c(temps$t[244:365], temps$t[365], temps$t[1:121]) + 5
# 

TA <- night %>% 
  group_by(julian.bird) %>% 
  summarize(jb = julian.bird[1],
            m_at = mean(airtemp, na.rm = T)) %>% 
  arrange(jb) %>% 
  pull(m_at)


# MODELS 

# Set params

# size and shape
AMASS <- 0.1  # mass (kg)
SHAPE <- 4 # ellipsoid
SHAPE_B <- 2  # ratio length/width
SHAPE_B_MAX <- 2.5  # maximum ratio of length to width/depth
SAMODE = 1 # bird surface area allomaetry

# feather properties - these are straigh from vignette (but modify fcn defaults) 
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


# TIMACT = 2 # multiplier on metabolic rate for activity costs

# Run models
# 
# Separate model for each strategy, iterate across timeseries of body temp data
# 
# fm
endo <- lapply(1:length(temp), function(x) {
  endoR(TC = temp[x],
        TA = TA[x],
        AMASS = AMASS,
        SHAPE = SHAPE,
        SHAPE_B = SHAPE_B,
        SHAPE_B_MAX = SHAPE_B_MAX,
        SAMODE = SAMODE,
        
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
        
        # add metabolic multiplier
        TIMACT = 1
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

# PLOTS #

# Plot body temp by time
plot(1:length(temp), temp, type = "l", 
     # ylim = c(40.5, 41.8),
     ylab = "Body Temp. (C)", xlab = "Julian Bird")


# Plot ambient temp by time
plot(1:length(TA), TA, 
     # type = "l", 
     # ylim = c(-1, 20),
     ylab = "Ambient Temp. (C)", xlab = "Julian Bird")


hr <- heartrate1 %>% 
  arrange(julian.bird) %>% 
  mutate(QGEN = enbal$QGEN)%>% 
  pull(mhr)

plot(1:length(hr), hr, type = "l", 
     # ylim = c(370,490),
     ylab = "Heart rate", xlab = "Julian Bird")

#MATCH to models

# Modeled met rate by time (this **should** by proportional to the heart rate data...)
plot(1:length(temp), enbal$QGEN, type = "l", 
     # ylim = c(1.3, 2.5),
     ylab = "Metabolic Rate (W)", xlab = "Julian Bird")

# Met rate by temp body temp
plot(temp, enbal$QGEN, 
     # ylim= c(1.3, 2.5), xlim = c(40.5,41.8),
     ylab = "Metabolic Rate (W)", xlab = "Body Temp (C)")

# Met rate by temp ambient temp
plot(TA, enbal$QGEN,
     ylab = "Metabolic Rate (W)", xlab = "Ambient Temp (C)")

## Regress Met and HR

hrcomb <- data.frame(hr = hr, met = enbal$QGEN)

ggplot(hrcomb, aes(x=hr, y = met)) +
  geom_point()+
  geom_smooth(method = "lm")

ggplot(hrcomb, aes(x = mhr, color = strat))+
  geom_density()
