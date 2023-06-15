library(mgcv)

night_out_df <- readRDS("out/night_out.rds")

# HEART RATE ~ MET 

#hr ~ met
fm.hr <-gam(heartrate ~ s(julian.bird) + met + s(ring, bs = "re") + s(ring, met, bs = "re"), 
            data = night_out_df)

# met ~ hr
fm.m <-gam(met ~ s(julian.bird) + heartrate + s(ring, bs = "re") + s(ring, heartrate, bs = "re"), 
           data = night_out_df)

fm.m2 <-gam(met ~ s(julian.bird) + s(heartrate) + s(ring, bs = "re") + s(ring, heartrate, bs = "re"),
            data = night_out_df)

fm.0 <-gam(met ~  s(heartrate) + s(ring, bs = "re") + s(ring, heartrate, bs = "re"),
           data = night_out_df)

out <- list(
  "data" = night_out_df,
  "fm.hr" = fm.hr,
  "fm.m" = fm.m,
  "fm.m2" = fm.m2,
  "fm.0" = fm.0
)

saveRDS(out, "out/night_mods_fitted.rds")