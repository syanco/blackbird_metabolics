#load output from night_ind.r and met_model_full.r (not implemented yet)


# convert HR to met

#observed met from hr

nd <- out.obs %>% 
  # filter(strat == "ws") %>% 
  # select(heartrate, juian.bird) %>% 
  rename(hrt = heartrate) 

p <- predict(fm.m,
                newdata = nd,
                # exclude = c("s(logger.id,hrt)","s(logger.id)"),
                # type = "terms"
                # iterms.type = 2
)

pred <- cbind(nd, p) %>% 
  mutate(tot_min_therm = p-metab,
         jbf = as.factor(julian.bird)) #calc dif between hrt-based total met and nichemapr-based thermo met

#summarize across individuals (within each strategy-day)
pred.sum <- pred %>% 
  group_by(strat, julian.bird) %>% 
  summarize(mhrt = mean(hrt),
          mp = mean(p),
          mmet = mean(metab),
          mtemp = mean(temp),
          mdiff = mean(tot_min_therm))

ggplot(pred, aes(x = julian.bird, y = tot_min_therm, group = band, color = strat))+
  geom_line()+
  geom_hline(yintercept = 0)

ggplot(pred.sum)+
  geom_line(aes(x=julian.bird, y = mmet, color = strat)) +
  geom_line(aes(x=julian.bird, y = mp, color = strat), linetype="dashed") +
  theme_minimal()  

ggplot(pred.sum)+
  geom_line(aes(x=julian.bird, y = mhrt, color = strat)) +
  # geom_line(aes(x=julian.bird, y = mp, color = strat), linetype="dashed") +
  theme_minimal()  

ggplot(pred.sum)+
  geom_line(aes(x=julian.bird, y = mdiff, color = strat)) +
  geom_hline(yintercept = 0) + 
  theme_minimal()  

ggplot(pred)+
  geom_boxplot(aes(x = jbf, y = tot_min_therm, color = strat )) +
  theme_minimal()

ggplot(pred)+
  geom_boxplot(aes(x = jbf, y = hrt, color = strat )) +
  theme_minimal()

# 
# nd.obs <- heartrate %>% 
#   ungroup() %>% 
#   select(heartrate, julian.bird, strat) %>% 
#   rename(hrt = heartrate, juian.bird = julian.bird) %>% 
#   mutate(logger.id = night$logger.id[23])
# 
# 
# 
# # data.frame(
# # hrt = seq(min(out.df$hrt), max(out.df$hrt), by = 0.001),
# # juian.bird = 100,
# # logger.id = out.df$logger.id[1])
# 
# p.obs <- predict(fm.m,
#                  newdata = nd.obs,
#                  exclude = c("s(logger.id,hr)")
#                  # type = "terms"
#                  # iterms.type = 2
# )
# 
# obs.df <- heartrate %>% 
#   ungroup() %>% 
#   mutate(p = p.obs)
# 
# obs.met.comp <- obs.df %>% 
#   group_by(julian.bird, strat) %>% 
#   summarise(mu_met = mean(p)) %>% 
#   ungroup()
# 
# ggplot(obs.met.comp) + 
#   geom_line(aes(x=julian.bird, y= mu_met, color = strat))
# ggplot(heartrate)+
#   geom_line(aes(x=julian.bird, y= mhr, color = strat))
# 
# 
# 
# obs.met.dif <- obs.met.comp %>% 
#   pivot_wider(id_cols = julian.bird, names_from = strat, values_from = mu_met) %>% 
#   mutate(met_dif = ws-fm)
# ggplot(obs.met.dif, aes(x=julian.bird, y= met_dif)) + 
#   geom_line()+
#   # geom_smooth() +
#   geom_hline(aes(yintercept = 0))
# 
# obs.met.comp %>% 
#   arrange(strat, julian.bird) %>% 
#   mutate(therm_met = enbal$QGEN) %>% 
#   rename(therm = therm_met, total = mu_met) %>% 
#   pivot_longer(cols = c(therm, total),
#                names_to = "component") %>% 
#   ggplot()+
#   geom_line(aes(x=julian.bird, y = value, color = component))+
#   facet_wrap(~strat)
