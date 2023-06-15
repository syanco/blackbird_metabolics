# Graphic examples

#Site comparison
ggplot() +
  geom_line(data = weatheryears[loc == "overwinter", ], aes(bird.year, mean.temp.year), alpha = 0.5, size = 0.70, col = "orange") +
  geom_point(data = weatheryears[loc == "overwinter", ], aes(x = bird.year, y = mean.temp.year), alpha = 0.75, size = 1.5, shape = 1, col = "orange") +
  geom_ribbon(data = weatheryears[loc == "overwinter", ], aes(x = bird.year, ymin = lower.temp, ymax = upper.temp, fill = "blue"), color = NA, alpha = 0.2, linetype = 3) +
  geom_line(data = weatheryears[loc == "breeding", ], aes(bird.year, mean.temp.year), alpha = 0.5, size = 0.70, col = "blue") +
  geom_point(data = weatheryears[loc == "breeding", ], aes(x = bird.year, y = mean.temp.year), alpha = 0.75, size = 1.5, shape = 1, col = "blue") +
  geom_ribbon(data = weatheryears[loc == "breeding", ], aes(x = bird.year, ymin = lower.temp, ymax = upper.temp, fill = "orange"), color = NA, alpha = 0.2, linetype = 3) +
  geom_line(data = weatheryears[loc == "breeding", ], aes(bird.year, temp.diff), alpha = 0.25, size = 0.70, col = "black") +
  geom_point(data = weatheryears[loc == "breeding", ], aes(x = bird.year, y = temp.diff), alpha = 0.75, size = 1.5, shape = 1, col = "black") +
  annotate(geom = "text", x = weatheryears[loc == "breeding", ]$bird.year, y = weatheryears[loc == "breeding", ]$temp.diff + 0.5, label = round(weatheryears[loc == "breeding", ]$temp.diff, 1), color = "black", size = 4) +
  scale_x_continuous(breaks = seq(min(weatheryears$bird.year), max(weatheryears$bird.year), by = 5)) +
  scale_y_continuous(breaks = seq(-7.5, 12.5, 2.5), limits = c(-8.4, 10)) +
  scale_fill_manual(values = c("orange", "blue"), name = "Temperature + CI", labels = c("Overwintering Region", "Breeding Region")) +
  scale_color_manual(values = c("orange", "blue"), name = "Temperature + CI", labels = c("Overwintering Region", "Breeding Region")) +
  labs(x = "Year", y = "Mean temperature [°C]") +
  theme_bw()+ theme(legend.position="bottom")

# Calendar Plot HRT
ggplot() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 7), axis.title = element_text(), legend.position = "bottom") +
  theme(axis.text = element_text(colour = "black")) +
  theme(legend.position = "bottom") +
  xlab("Day of the year") +
  ylab("Heart rate  [bpm]") +
  geom_rect(aes(xmin = max(dep.mig.nights[julian.bird.shifted < 147]$julian.bird.shifted), xmax = min(dep.mig.nights[julian.bird.shifted > 147]$julian.bird.shifted), ymin = min.fh.day, ymax = Inf), fill = "grey", alpha = 0.2) +
  geom_ribbon(data = output.selection, aes(x = julian.bird.shifted, ymin = lower.fh, ymax = upper.fh, group = (strat), fill = (strat)), color = NA, alpha = 0.25, linetype = 3, show.legend = FALSE) +
  scale_color_manual(labels = (c("Migrants    ", "Residents    ")), values = rev(c("#5FADE8", "#F29C67"))) +
  scale_fill_manual(values = rev(c("#5FADE8", "#F29C67"))) +
  geom_line(data = output.selection, aes(julian.bird.shifted, mean.fh, group = strat, col = (strat)), alpha = 0.9, size = 0.5) +
  scale_y_continuous(breaks = seq(min.fh.day + 50, max.fh.day - 50, 50), limits = c(min.fh.day, max.fh.day), expand = c(0, 0),sec.axis = sec_axis(~. *1,breaks=hrt.breaks[-1],labels = hrt.labels[-1],name = "# Migrating individuals")
  ) +
  scale_x_continuous(breaks = seq(start, end-7, 7), labels = labelsnew[-length(labelsnew)], expand = c(0.005, 0.005), limits = c(49, 271)) +
  theme(legend.title = element_blank()) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, shape = 95, size = 7))) +
  geom_segment(data = dep.mig.nights, aes(x = julian.bird.shifted, xend = julian.bird.shifted, y = min.fh.day, yend = scaled.hrt.day.n.migrating.all), size = 1.25, col = "black", alpha = 0.30) +
  geom_segment(data = dep.mig.nights, aes(x = julian.bird.shifted, xend = julian.bird.shifted, y = min.fh.day, yend = scaled.hrt.day.n.migrating.init), size = 1.25, col = "black", alpha = 0.30) +
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 14, face = "bold")) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(12, 0, 0, 0))) +
  theme(text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15), axis.title.x = element_blank())+
  theme(axis.title.y.right = element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l =10)))

# Centered Plots
ggplot() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 7), axis.title = element_text(), legend.position = "bottom") +
  theme(axis.text = element_text(colour = "black")) +
  theme(legend.position = "bottom") +theme(legend.title = element_blank()) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, shape = 95, size = 7))) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black", size = 0.5, alpha = 0.9) +
  scale_color_manual(labels = (c("Migrants    ", "Residents    ")), values = rev(c("#5FADE8", "#F29C67"))) +
  scale_fill_manual(values = rev(c("#5FADE8", "#F29C67"))) +
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 14, face = "bold")) +
  theme(text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15), axis.title.x = element_blank())+
xlab("Days relative to departure") +
  ylab("Heart rate  [bpm]") +
  geom_rect(aes(xmin = 0-0.5, xmax = end+0.5, ymin = min.fh.day, ymax = Inf), fill = "grey", alpha = 0.2) +
  scale_y_continuous(breaks = seq(350 , 550 , 50), limits = c(min.fh.day, max.fh.day), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(start, end, 7), expand = c(0, 0), limits = c(start - 0.5, end +0.5)) +
  annotate(geom = "text", x = -0.5,hjust = 1, y = max.fh.day - 20, label = "Departure\nBreeding Site", color = "black", size = 5) +
  annotate(geom = "text", x = start+7,hjust = 1, y = min.fh.day + 70, label = "", color = "black", size = 5) +
  geom_segment(mapping=aes(x=prepstart.hrt.day-0.3, y=min.fh.day+4+6, xend=-1+0.5, yend=min.fh.day+4+6), arrow=arrow(length = unit(0.03, "npc"), type="closed" ), size=0.5, color="red") + 
  geom_rect(mapping=aes(xmin=prepstart.hrt.day-0.3, ymin=min.fh.day+4, xmax=prepstart.hrt.day-0.3, ymax=min.fh.day+4+12),  size=0.5, color="red") + 
  geom_point(data = output.selection, aes(x = xdays, y = mean.fh, group = strat, col = (strat)),position = position_dodge(0.4), size = 2,) +
  geom_errorbar(data = output.selection, aes(x = xdays, ymin = lower.fh, ymax = upper.fh, group = strat, col = (strat)),position = position_dodge(0.4), size = 1, width = 0)+
  geom_line(data = data.table(preds.hrt.fallpremig$fv)[strat_phase %in% c("fm_day","ws_day")], aes(xdays, fit,group = strat_phase),col=farbe, alpha = 0.7, size = 1) 

