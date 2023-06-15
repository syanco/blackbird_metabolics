runMod <- function(dat){
  
  # Run endo therm model 
  endo <- lapply(1:nrow(dat), function(x) {
    endoR(WRITE_INPUT = 1,
          TC = dat$bodytemp[x],
          TA = dat$airtemp[x],
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
    ) #endoR
  }) #lapply
  
  # extract the output
  endo.out <- do.call("rbind", lapply(endo, data.frame))
  
  # heat balance
  enbal <- endo.out[, grep(pattern = "enbal", colnames(endo.out))]
  colnames(enbal) <- gsub(colnames(enbal), pattern = "enbal.", 
                          replacement = "")
  
  # create output df including observed data
  tmp <- data.frame(heartrate = dat$heartrate, 
                    met = enbal$QGEN,
                    airtemp = dat$airtemp,
                    bodytemp = dat$bodytemp,
                    julian.bird = dat$julian_bird,
                    ring = dat$ring,
                    strat = dat$strat
  )
  
  return(tmp)
}