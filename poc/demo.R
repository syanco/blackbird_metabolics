library(NicheMapR)
library(tidyverse)
library(ggplot2)

#basic
endo.out <- endoR(TA = 0)

load("data/Blackbird.RData")

bbtemp <-bodytemperature %>% 
  group_by(julian.regular) %>% 
  summarise(mbt = mean(bodytemp)) %>% 
  pull(mbt)

#start from vignette settings and modify:
# environment
TAs <- 20  # air temperature (deg C)
VEL <- 0.000  # wind speed (m/s)
vd <- WETAIR(rh = 30, db = 40)$vd  # Weather and Schoenbaechler had 16.7 mm Hg above 40 deg C = 30% RH at 40 deg C
vd_sat <- WETAIR(rh = 100, db = TAs)$vd  # Weather and Schoenbaechler had 16.7 mm Hg above 40 deg C = 30% RH at 40 deg C
exp_rh <- vd/vd_sat * 100
exp_rh[exp_rh > 100] <- 100
exp_rh[TAs < 30] <- 15
hum <- exp_rh  #rep(humidity,96)

# core temperature
TC <- bbtemp  # core temperature (deg C)
TCMAX <- 43  # maximum core temperature (deg C)
RAISETC <- 0.25  # increment by which TC is elevated (deg C)

# size and shape
AMASS <- 0.1  # mass (kg)
SHAPE_B <- 2  # ratio length/width
SHAPE_B_MAX <- 5  # maximum ratio of length to width/depth
UNCURL <- 0.1  # allows the animal to uncurl to SHAPE_B_MAX, the value being the increment SHAPE_B is increased per iteration
SHAPE <- 4  # use ellipsoid geometry
SAMODE <- 1  # use bird surface area relations (2 is mammal, 0 is based on shape specified in GEOM)

# feather properties
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

# physiological responses
SKINW <- 0.5  # base skin wetness (%)
MXWET <- 5  # maximum skin wetness (%)
SWEAT <- 0.25  # intervals by which skin wetness is increased (%)
Q10s <- rep(1, length(TAs))
Q10s[TAs >= TCMAX] <- 2  # assuming Q10 effect kicks in only after air temp rises above TCMAX
QBASAL <- 10^(-1.461 + 0.669 * log10(AMASS * 1000))  # basal heat generation (W)
DELTAR <- 5  # offset between air temeprature and breath (°C)
EXTREF <- 25  # O2 extraction efficiency (%)
PANTING <- 0.1  # turns on panting, the value being the increment by which the panting multiplier is increased up to the maximum value, PANTMAX
PANTMAX <- 15  # maximum panting rate - multiplier on air flow through the lungs above that determined by metabolic rate

endo.out <- lapply(1:length(TC), function(x) {
  endoR(TA = TAs, 
        VEL = VEL,
        TC = TC[x],
        TC_MAX = TCMAX,
        # RH = hum[x],
        AMASS = AMASS,
        SHAPE = SHAPE,
        SHAPE_B = SHAPE_B_REF,
  )
})

ptm <- proc.time()  # start timing
endo.out <- lapply(1:length(TAs), function(x) {
  endoR(TA = TAs[x], 
        VEL = VEL,
        TC = TC,
        TC_MAX = TCMAX,
        RH = hum[x],
        AMASS = AMASS,
        SHAPE = SHAPE,
        SHAPE_B = SHAPE_B_REF,
        # SHAPE_B_MAX = SHAPE_B_MAX,
        # SKINW = SKINW,
        # SWEAT = SWEAT,
        # MXWET = MXWET,
        # Q10 = Q10s[x],
        # QBASAL = QBASAL,
        # DELTAR = DELTAR,
        # DHAIRD = DHAIRD,
        # DHAIRV = DHAIRV,
        # LHAIRD = LHAIRD,
        # LHAIRV = LHAIRV,
        # ZFURD = ZFURD,
        # ZFURV = ZFURV,
        # RHOD = RHOD,
        # RHOV = RHOV,
        # REFLD = REFLD,
        # RAISETC = RAISETC,
        # PANTING = PANTING,
        # PANTMAX = PANTMAX,
        # EXTREF = EXTREF,
        # UNCURL = UNCURL,
        # SAMODE = SAMODE
  )
})  # run endoR across environments
proc.time() - ptm  # stop timing

# PLOTS from Kearney
# extract the output
endo.out1 <- do.call("rbind", lapply(endo.out, data.frame))

# thermoregulation output
treg <- endo.out1[, grep(pattern = "treg", colnames(endo.out1))]
colnames(treg) <- gsub(colnames(treg), pattern = "treg.", replacement = "")

# morphometric output
morph <- endo.out1[, grep(pattern = "morph", colnames(endo.out1))]
colnames(morph) <- gsub(colnames(morph), pattern = "morph.", 
                        replacement = "")

# heat balance
enbal <- endo.out1[, grep(pattern = "enbal", colnames(endo.out1))]
colnames(enbal) <- gsub(colnames(enbal), pattern = "enbal.", 
                        replacement = "")

# mass aspects
masbal <- endo.out1[, grep(pattern = "masbal", colnames(endo.out1))]
colnames(masbal) <- gsub(colnames(masbal), pattern = "masbal.", 
                         replacement = "")

QGEN <- enbal$QGEN  # metabolic rate (W)
H2O <- masbal$H2OResp_g + masbal$H2OCut_g  # g/h water evaporated
TFA_D <- treg$TFA_D  # dorsal fur surface temperature
TFA_V <- treg$TFA_V  # ventral fur surface temperature
TskinD <- treg$TSKIN_D  # dorsal skin temperature
TskinV <- treg$TSKIN_V  # ventral skin temperature
TCs <- treg$TC  # core temperature

par(mfrow = c(2, 2))
par(oma = c(2, 1, 2, 2) + 0.1)
par(mar = c(3, 3, 1.5, 1) + 0.1)
par(mgp = c(2, 1, 0))
plot(QGEN ~ TAs, type = "l", ylab = "metabolic rate, W", xlab = "air temperature, deg C", 
     ylim = c(0.2, 1.2))
points(Weathers1976Fig1$Tair, Weathers1976Fig1$mlO2gh * 20.1/3600 * 
         (AMASS * 1000), pch = 16, col = 2)
legend(x = 30, y = 1.2, legend = c("observed", "predicted"), 
       col = c("red", "black"), lty = c(NA, 1), bty = "n", pch = c(16, 
                                                                   NA))
plot(H2O ~ TAs, type = "l", ylab = "water loss, g/h", xlab = "air temperature, deg C", 
     ylim = c(0, 1.5))
points(masbal$H2OResp_g ~ TAs, type = "l", lty = 2)
points(masbal$H2OCut_g ~ TAs, type = "l", lty = 2, col = "blue")
legend(x = 3, y = 1.5, legend = c("total (obs)", "total (pred)", 
                                  "respiratory", "cutaneous"), col = c("red", "black", "black", 
                                                                       "blue"), lty = c(NA, 1, 2, 2), bty = "n", pch = c(16, NA, 
                                                                                                                         NA, NA))
points(Weathers1976Fig3$Tair, Weathers1976Fig3$mgH2Ogh * AMASS, 
       pch = 16, col = 2)
plot(TFA_D ~ TAs, type = "l", col = "grey", ylab = "feather, skin and core temperature, deg C", 
     xlab = "air temperature, deg C", ylim = c(10, 50))
points(TFA_V ~ TAs, type = "l", col = "grey", lty = 2)
points(TskinD ~ TAs, type = "l", col = "orange")
points(TskinV ~ TAs, type = "l", col = "orange", lty = 2)
points(TCs ~ TAs, type = "l", col = "red")
legend(x = 30, y = 33, legend = c("core (obs)", "core (pred)", 
                                  "skin dorsal", "skin ventral", "feathers dorsal", "feathers ventral"), 
       col = c("red", "red", "orange", "orange", "grey", "grey"), 
       lty = c(NA, 1, 1, 2, 1, 2), bty = "n", pch = c(16, NA, NA, 
                                                      NA, NA, NA))
points(Weathers1976Fig2$Tair, Weathers1976Fig2$Tb, pch = 16, 
       col = 2)
plot(masbal$AIR_L * 1000/60 ~ TAs, ylim = c(0, 250), lty = 1, 
     xlim = c(-5, 50), ylab = "ml / min", xlab = paste("air temperature (deg C)"), 
     type = "l")
legend(x = 0, y = 250, legend = c("observed", "predicted"), col = c("red", 
                                                                    "black"), lty = c(NA, 1), bty = "n", pch = c(16, NA))
points(Weathers1976Fig5$breaths_min * (13.2 * AMASS^1.08) * ((Weathers1976Fig5$Tair + 
                                                                273.15)/273.15) ~ Weathers1976Fig5$Tair, col = "red", pch = 16)  # tidal volume allometry from Lasiewski, R. C., and W. A. Calder. 1971, correcting volume according to PV = nRT equation, where V_2 = T_2 * V_1 / T_2, and T_1 is at STP, so 0 deg C


############

# Micro Climate Demo

#run once
# get.global.climate(folder = 'data/clim/')

loc <- c(47.77, 08.99)
Usrhyt <- 0.5
maxshade <- 100
micro <- micro_global(loc = loc, Usrhyt = Usrhyt, maxshade = maxshade)

metout <- as.data.frame(micro$metout)  # unshaded above-ground conditions
soil <- as.data.frame(micro$soil)  # unshaded below-ground soil temperatures
shadmet <- as.data.frame(micro$shadmet)  # shaded above-ground conditions
shadsoil <- as.data.frame(micro$shadsoil)  # shaded below-ground soil temperatures
dates <- micro$dates

TAs <- metout$TALOC  # air temperatures at height of animal (deg C)
TAREFs <- metout$TAREF  # air temperatures at reference height (deg C)
TSKYs <- metout$TSKYC  # sky temperatures (deg C)
TGRDs <- soil$D0cm  # surface temperatures (deg C)
VELs <- metout$VLOC  # wind speeds at animal height (m/s)
RHs <- metout$RHLOC  # relative humidity at animal height (%)
QSOLRs <- metout$SOLR  # solar radiation (W/m2)
Zs <- metout$ZEN  # zenith angle of the sun (degrees)
ELEV <- micro$elev  # elevation (m)
ABSSB <- 1 - micro$REFL  # substrate solar absorptivity (%)