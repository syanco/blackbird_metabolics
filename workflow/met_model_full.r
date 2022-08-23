# LIBRARIES #

library(NicheMapR)
library(tidyverse)

# DATA #
# 
# Load Blackbird data
load("data/Blackbird.RData")

# Solar model
loc <- c(8.966910, 47.745237) # use location of Radolfzell for all birds
  #TODO: this is not perfect but is conservative wrt differences btw strategies
Usrhyt <- 1 # height for met = 1m

# run microclimate model
# run once to download data
# get.global.climate(folder = 'data/')
micro <- micro_global(loc = loc, Usrhyt = Usrhyt, solonly = 1, timeinterval = 365)

# extract solar components
metout <- as.data.frame(micro$metout) %>% 
  group_by(DOY) %>% 
  summarize(SOLR = mean(SOLR),
            mt = mean(TALOC))

# make master dataframe to feed into endo model
dat <- bodytemperature %>% 
  full_join(heartrate) %>% 
  full_join(weather, by = c("strat", "julian.bird")) %>% 
  # left_join(si_47, by = c("julian.bird" = "day")) # only using SI from Germany for now - #TODO = need to make similar to airtemp
  left_join(metout, by = c("julian.bird" = "DOY"))

# INITS #

# size and shape
AMASS <- 0.1  # mass (kg)
SHAPE <- 4 # ellipsoid
SHAPE_B <- 2  # ratio length/width
SHAPE_B_MAX <- 3  # maximum ratio of length to width/depth
SAMODE = 1 # bird surface area allomaetry
TC_MAX <- 45

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

# Behavior
PCTWET_INC = 0

# Morp
ANDENS = 730 # body denisty kg/m3
SUBQFAT = 1 # subQ fat is present

# MODELS #

# Set vector of individuals over which to loop
inds <- unique(dat$ring)

# Init empty list to store results
out <- list()

# Loop over individuals, run endo model
for(i in 1: length(inds)){
  
  dat1 <- dat %>% 
    filter(ring == inds[i])%>% 
    na.omit
  
  
  if(nrow(dat1) > 0){
    
    endo <- lapply(1:nrow(dat1), function(x) {
      endoR(WRITE_INPUT = 1,
            TC = dat1$bodytemp[x],
            TA = dat1$airtemp[x],
            QSOLR = dat1$SOLR[x],
            TC_MAX = TC_MAX,
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
            
            # behavior
            PCTWET_INC = PCTWET_INC,
            
            # morph
            ANDENS = ANDENS,
            SUBQFAT = SUBQFAT,
            
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
    # store results in df with input data
    tmp <- data.frame(heartrate = dat1$heartrate, 
                      metab = enbal$QGEN,
                      airtemp = dat1$airtemp,
                      temp = dat1$bodytemp,
                      julian.bird = dat1$julian.bird,
                      band = dat1$ring,
                      strat = dat1$strat,
                      logger.id = dat1$ring
    )
    
    out[[i]] <- tmp
  } else {
    next
    # out[[i <- NULL]]
  } # if nrow > 0
} #i

# Collate results
tot_out_df <- do.call("rbind", out) 


# WRITE OUT

save.image(file = "out/tot_met.rdata")