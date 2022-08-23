####################
#                  #
#  Demo DEB Model  #
#                  #
####################

#---- Load DEB Params ----#
# library(R.matlab)
# allStat <- readMat('data/allStat.mat') # this will take a few minutes
# save(allStat, file = '../data/allstat.Rda') # save it as an R data file for faster future loading
load("data/data_amp2021-01-28.rdata")
allStat <- deb
library(knitr) # this packages has a function for producing formatted tables.
# load('allStat.Rda')

allDEB.species<-unlist(labels(allStat$allStat)) # get all the species names
allDEB.species<-allDEB.species[1:(length(allDEB.species)-2)] # last two elements are not species names
kable(head(allDEB.species))
Nspecies <- length(allStat$allStat)
Nspecies
# species <- "Turdus.merula"
species <- "Turdus.migratorius"
species.slot <- which(allDEB.species == species)
par.names <- unlist(labels(allStat$allStat[[species.slot]]))

for(i in 1:length(par.names)){
  assign(par.names[i], unlist(allStat$allStat[[species.slot]][i]))
}

#---- Run DEB ----#
library(NicheMapR)
species <- "Turdus.merula" # must be in the AmP collection - see allDEB.species list
ndays <- 365*5 # number days to run the simulation for
div <- 1 # time step divider (1 = days, 24 = hours, etc.) - keep small if using Euler method
Tbs <- rep(25, ndays * div) # °C, body temperature
starvetime <- 0 # length of low food period when simulating starvation
X <- 1000 # J/cm2 base food density
# TODO: apples, berries, insects 
Xs <- c(rep(X, ndays * div / 2), rep(0.000005, starvetime), 
        rep(X, ndays * div / 2 - starvetime + 1)) # food density (J/cm2 or J/cm3)
E_sm <- 3000 # J/cm3, volume-specific stomach energy
clutchsize <- 2 # -, clutch size

mass.unit <- 'g'
length.unit <- 'mm'
plot <- 1 # plot results?
start.stage <- 1 # stage in life cycle to start (0 = egg, 1 = juvenile, 2 = puberty)

deb_fit <- rundeb(species = species, ndays = ndays, div = div, Tbs = Tbs, clutchsize = clutchsize, 
              Xs = Xs, mass.unit = mass.unit, length.unit = length.unit, 
              start.stage = start.stage, E_sm = E_sm, plot = plot, stages = 2)
