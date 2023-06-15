bodytemperature1 <- bodytemperature %>% 
  mutate(jplus = julian.regular-243,
         jminus = julian.regular+123,
         jcon = case_when(jplus > 0 ~ jplus,
                          jminus > 0 ~ jminus))

ggplot(bodytemperature1)+
  geom_line(aes(x=jcon, y=bodytemp, group = strat, color = strat))

heartrate1 <- heartrate %>% 
  mutate(jplus = julian.regular-243,
         jminus = julian.regular+123,
         jcon = case_when(jplus > 0 ~ jplus,
                          jminus > 0 ~ jminus)) %>% 
  group_by(jcon, strat) %>% 
  summarise(mhr = mean(heartrate)) 

hrws <- heartrate1 %>% 
  ungroup() %>% 
  filter(strat == "ws") %>% 
  mutate(QGEN = enbal.ws$QGEN)

ggplot(hrws,aes(x=mhr, y = QGEN)) +
  geom_point()+
  geom_smooth(method = "lm")

