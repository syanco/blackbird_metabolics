load(file = "out/night_reg.rdata")

library(ggplot2)
library(ggthemes)
library(gratia)
library(patchwork)

a <- ggplot(night_out_df, aes(x=hrt, y = met, color = logger.id))+
  geom_point() +
  theme(legend.position = "none")

b <- ggplot(night_out_df, aes(x=julian.bird, y = met, color = logger.id))+
  geom_point() +
  theme(legend.position = "none")

a/b + plot_annotation(tag_levels = 'A')

plot(fm.m2, all.terms =T, ylab = "Watts", shift = coef(fm.m2)[1])
