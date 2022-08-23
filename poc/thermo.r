####################################
####                            ####
####          thermo.r          ####
####      Scott Yanco, PhD      ####
####    scott.yanco@yale.edu    ####
####                            ####
####################################


# Script to run `nichemapr` thermoregulation model on blackbirds using observed
# body temperature and observed/modeled ambient temperature

##--  LIBRARIES  --##

library(NicheMapR)
library(tidyverse)

##--  DATA  --##

# Load Blackbird data
load("data/Blackbird.RData")


##--  INITS  --##

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



##--  PROCESS DATA  --##

# body temps
bbtemp <- bodytemperature %>%
  #create "day" variable that starts at beginning of tracking instead of Jan 1
  # mutate(jplus = julian.regular-243,
  #        jminus = julian.regular+123,
  #        jcon = case_when(jplus > 0 ~ jplus,
  #                         jminus > 0 ~ jminus))%>%
  # get daily means for each strategy
  group_by(julian.bird, strat) %>%
  summarise(mbt = mean(bodytemp))
#TODO: cutoff temps at end of good data - julian day: 10 april (DOY 90)

# extract vector for fm
fmtemp <- bbtemp %>%
  filter(strat == "fm") %>%
  arrange(julian.bird) %>%
  pull(mbt)

# extract vector for ws
wstemp <- bbtemp %>%
  filter(strat == "ws") %>%
  arrange(julian.bird) %>%
  pull(mbt)

# Heart rate
heartrate1 <- heartrate %>%
  # #create tracking day variable
  # mutate(jplus = julian.regular-243,
  #        jminus = julian.regular+123,
  #        jcon = case_when(jplus > 0 ~ jplus,
  #                         jminus > 0 ~ jminus)) %>%
  # get daily means for each strategy
  group_by(julian.bird, strat) %>%
  summarise(mhr = mean(heartrate)) %>%
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

TAws <- weather %>%
  filter(strat == "ws") %>%
  arrange(julian.bird) %>%
  pull(airtemp)

TAfm <- weather %>%
  filter(strat == "fm") %>%
  arrange(julian.bird) %>%
  pull(airtemp)

# MODELS

inds <- levels(bodytemperature$logger.id)

# i <- 7
# process data
#
# body temps

out <- list()

for(i in 1: length(inds)){
  

# TIMACT = 2 # multiplier on metabolic rate for activity costs

# Run models
#
# Separate model for each strategy, iterate across timeseries of body temp data
#
# fm
endo.fm <- lapply(1:length(fmtemp), function(x) {
  endoR(
    TC = fmtemp[x],
    TA = TAfm[x],
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

# ws
endo.ws <- lapply(1:length(wstemp), function(x) {
  endoR(
    TC = wstemp[x],
    TA = TAws[x],
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
endo.out.fm <- do.call("rbind", lapply(endo.fm, data.frame))
endo.out.ws <- do.call("rbind", lapply(endo.ws, data.frame))

# heat balance
enbal.fm <-
  endo.out.fm[, grep(pattern = "enbal", colnames(endo.out.fm))]
colnames(enbal.fm) <-
  gsub(colnames(enbal.fm),
       pattern = "enbal.",
       replacement = "")
enbal.ws <-
  endo.out.ws[, grep(pattern = "enbal", colnames(endo.out.ws))]
colnames(enbal.ws) <-
  gsub(colnames(enbal.ws),
       pattern = "enbal.",
       replacement = "")

# PLOTS #

# Plot body temp by time
plot(
  1:length(fmtemp),
  fmtemp,
  type = "l",
  ylim = c(40.5, 41.8),
  ylab = "Body Temp. (C)",
  xlab = "Julian Bird"
)
lines(1:length(wstemp), wstemp, col = "red")


# Plot ambient temp by time
plot(
  1:length(TAfm),
  TAfm,
  type = "l",
  ylim = c(-1, 20),
  ylab = "Ambient Temp. (C)",
  xlab = "Julian Bird"
)
lines(1:length(TAws), TAws, col = "red")


# Modify the heart rate data for plotting
hrws <- heartrate1 %>%
  filter(strat == "ws") %>%
  arrange(julian.bird) %>%
  mutate(QGEN = enbal.ws$QGEN) %>%
  pull(mhr)

hrfm <- heartrate1 %>%
  filter(strat == "fm") %>%
  arrange(julian.bird) %>%
  mutate(QGEN = enbal.fm$QGEN) %>%
  pull(mhr)

plot(
  1:length(hrfm),
  hrfm,
  type = "l",
  ylim = c(370, 490),
  ylab = "Heart rate",
  xlab = "Julian Bird"
)
lines(1:length(hrws), hrws, col = "red")

#MATCH to models

# Modeled met rate by time (this **should** by proportional to the heart rate data...)
plot(
  1:length(fmtemp),
  enbal.fm$QGEN,
  type = "l",
  ylim = c(1.3, 2.5),
  ylab = "Metabolic Rate (W)",
  xlab = "Julian Bird"
)
lines(1:length(wstemp), enbal.ws$QGEN, col = "red")

# Met rate by temp body temp
plot(
  fmtemp,
  enbal.fm$QGEN,
  ylim = c(1.3, 2.5),
  xlim = c(40.5, 41.8),
  ylab = "Metabolic Rate (W)",
  xlab = "Body Temp (C)"
)
points(wstemp, enbal.ws$QGEN, col = "red")

# Met rate by temp ambient temp
plot(TAfm, enbal.fm$QGEN,
     ylab = "Metabolic Rate (W)", xlab = "Ambient Temp (C)")
points(TAws, enbal.ws$QGEN, col = "red")

# ggplot(hrcomb, aes(x=mhr, y = QGEN)) +
#   geom_point()+
#   geom_smooth(method = "lm") +
#   facet_wrap(~strat)
#
# ggplot(hrcomb, aes(x = mhr, color = strat))+
#   geom_density()
