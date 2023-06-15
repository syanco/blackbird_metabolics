library(tidyverse)
library(ggplot2)
library(mgcv)
library(patchwork)
library(tidymv)
library(ggthemes)
library(lubridate)


migrantColor <- "#FF7F50"  # Coral 
residentColor <- "#008080"  # Teal

birdPal <- c(migrantColor, residentColor)

# MEAN #

met_mean <- readRDS("out/tot_out_experienced_temp_mean.rds") %>% 
  filter(julian.bird < 271) %>% 
  mutate(strat = fct_relevel(strat, rev(levels(strat))))


fm_therm_mean <- gam(met ~ strat + s(julian.bird, by = strat) + s(ring, bs = "re"), 
                     data = met_mean)

preds_mean <- get_gam_predictions(fm_therm_mean, series = julian.bird,
                                  series_length = max(met_mean$julian.bird)-min(met_mean$julian.bird))

(mean_plot <- ggplot(preds_mean,
                     aes(x = julian.bird, y = met, color = strat, fill = strat)) +
    geom_line(alpha = 0.5)+
    geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, linetype = 3)+
    scale_fill_manual(values = birdPal, name = "Strategy", labels = c("Migrants", "Residents")) +
    scale_color_manual(values = birdPal, name = "Strategy", labels = c("Migrants", "Residents")) +
    # scale_linetype(guide = "none")+
    ylim(0.5, 2.8) +
    labs(x = "Julian Bird", y = "Watts") +
    # ggtitle("Mean Winter Temps")+
    theme_bw()+ theme())

ggsave(filename = "out/figs/main_met_plot.png", mean_plot)
# (mean_plot <- plot_smooths(fm_therm_mean, julian.bird, strat)+
#     scale_fill_manual(values = c("orange", "blue"), name = "Strategy", labels = c("Migrants", "Residents")) +
#     scale_color_manual(values = c("orange", "blue"), name = "Strategy", labels = c("Migrants", "Residents")) +
#     scale_linetype(guide = "none")+
#     ylim(0.5, 2.8) +
#     labs(x = "Julian Bird", y = "Watts") +
#     ggtitle("Mean Winter Temps")+
#     theme_bw()+ theme())


# MINIMUM #

met_min <- readRDS("out/tot_out_experienced_temp_minimum.rds") %>% 
  filter(julian.bird < 271) %>% 
  mutate(strat = fct_relevel(strat, rev(levels(strat))))


fm_therm_min <- gam(met ~ strat + s(julian.bird, by = strat) + s(ring, bs = "re"), 
                    data = met_min)

preds_min <- get_gam_predictions(fm_therm_min, series = julian.bird, 
                                  series_length = max(met_mean$julian.bird)-min(met_mean$julian.bird))

(min_plot <- ggplot(preds_min,
                     aes(x = julian.bird, y = met, color = strat, fill = strat)) +
    geom_line(alpha = 0.5)+
    geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, linetype = 3)+    
    scale_fill_manual(values = birdPal, name = "Strategy", labels = c("Migrants", "Residents")) +
    scale_color_manual(values = birdPal, name = "Strategy", labels = c("Migrants", "Residents")) +
    # scale_linetype(guide = "none")+
    ylim(0.5, 2.8) +
    labs(x = "Julian Bird", y = "Watts") +
    # ggtitle("Minimum Winter Temps")+
    theme_bw()+ theme())


# MAXIMUM #

met_max <- readRDS("out/tot_out_experienced_temp_maximum.rds") %>% 
  filter(julian.bird < 271) %>% 
  mutate(strat = fct_relevel(strat, rev(levels(strat))))


fm_therm_max <- gam(met ~ strat + s(julian.bird, by = strat) + s(ring, bs = "re"), 
                    data = met_max)

preds_max <- get_gam_predictions(fm_therm_max, series = julian.bird, 
                                  series_length = max(met_mean$julian.bird)-min(met_mean$julian.bird))

(max_plot <- ggplot(preds_max,
                     aes(x = julian.bird, y = met, color = strat, fill = strat)) +
    geom_line(alpha = 0.5)+
    geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, linetype = 3)+    
    scale_fill_manual(values = c("orange", "blue"), name = "Strategy", labels = c("Migrants", "Residents")) +
    scale_color_manual(values = c("orange", "blue"), name = "Strategy", labels = c("Migrants", "Residents")) +
    # scale_linetype(guide = "none")+
    ylim(0.5, 2.8) +
    labs(x = "Julian Bird", y = "Watts") +
    # ggtitle("Maximum Winter Temps")+
    theme_bw()+ theme())


# BREEDING

met_breed <- readRDS("out/tot_out_breedingsite_temp.rds") %>% 
  filter(julian.bird < 271) %>% 
  mutate(strat = fct_relevel(strat, rev(levels(strat))))

fm_therm_breed <- gam(met ~ strat + s(julian.bird, by = strat) + s(ring, bs = "re"), 
                      data = met_breed)

preds_breed <- get_gam_predictions(fm_therm_breed, series = julian.bird, 
                                  series_length = max(met_mean$julian.bird)-min(met_mean$julian.bird))

(breed_plot <- ggplot(preds_breed,
                     aes(x = julian.bird, y = met, color = strat, fill = strat)) +
    geom_line(alpha = 0.5)+
    geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, linetype = 3)+    
    scale_fill_manual(values = birdPal, name = "Strategy", labels = c("Migrants", "Residents")) +
    scale_color_manual(values = birdPal, name = "Strategy", labels = c("Migrants", "Residents")) +
    scale_linetype(guide = "none")+
    # ylim(0.5, 2.8) +
    labs(x = "Julian Bird", y = "Watts") +
    ggtitle("Counterfactual Metabolism")+
    theme_bw()+ theme(legend.position="none"))

plot_difference(fm_therm_breed, series = julian.bird, 
                series_length = max(met_mean$julian.bird)-min(met_mean$julian.bird),
                difference = list(strat = c("fm", "ws")),
                exclude = F)

# BODY TEMP #

(bodytemp_plot <- ggplot(data = met_breed, aes(x=julian.bird, y = bodytemp, 
                                               color = strat, fill = strat))+
    geom_smooth()+
    scale_fill_manual(values = birdPal, name = "Strategy", labels = c("Migrants", "Residents")) +
    scale_color_manual(values = birdPal, name = "Strategy", labels = c("Migrants", "Residents")) +
    scale_linetype(guide = "none")+
    # ylim(0.5, 2.8) +
    labs(x = "Julian Bird", y = "Watts") +
    ggtitle("Body Temps")+
    theme_bw()+ theme(legend.position="bottom"))

# AIRTEMP #
(airtemp_plot <- ggplot(met_breed, aes(x=julian.bird, y = airtemp, color = strat, fill = strat))+ 
  geom_smooth()+
  scale_fill_manual(values = birdPal name = "Strategy", labels = c("Migrants", "Residents")) +
  scale_color_manual(values = birdPal, name = "Strategy", labels = c("Migrants", "Residents")) +
  scale_linetype(guide = "none")+
  # ylim(0.5, 2.8) +
  labs(x = "Julian Bird", y = "Watts") +
  ggtitle("Air Temps")+
  theme_bw()+ theme(legend.position="bottom"))


# Combine Plots

layout <- c(
  area(t = 1, l = 1, b = 3, r = 3),
  area(t = 4, l = 1, b = 4.5, r = 1),
  area(t = 4, l = 2, b = 4.5, r = 2),
  area(t = 4, l = 3, b = 4.5, r = 3)
)

(mig_met_plot <- mean_plot + min_plot + max_plot + guide_area() + 
    plot_layout(guide = "collect", design = layout) + plot_annotation(tag_levels = c('A')))

ggsave(filename = "out/figs/winter_met_plot.png", mig_met_plot)

ggsave(filename = "out/figs/counterfactual_breeding_plot.png", breed_plot)


layout2 <- c(
  area(t = 1, l = 1, b = 2, r = 3),
  area(t = 3, l = 1, b = 4, r = 3),
  area(t = 5, l = 1, b = 6, r = 3),
  area(t = 7, l = 0, b = 7.5, r = 3)
)

(count_w_data <- airtemp_plot+bodytemp_plot+breed_plot + guide_area() + plot_layout(guide = "collect", design = layout2))
ggsave(filename = "out/figs/counterfactual_w_data.png")
# (mean_plot / (max_plot | min_plot))/guide_area() + plot_layout(guide = "collect", design = layout)
