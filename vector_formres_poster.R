#Save Output. 
#********************************************************************************#

#********************************************************************************#
#Full Abundance Model.
ab_coeff <- read.csv('Model Results/Abundance. Full. Coefficients.csv')

ab_coeff$Names <- c("Wall material (Cement)", "Roof material (Grass)", "Roof material (Other)", "Firewood use", 
                    "Coil/insectide use", "No. of rooms (3 - 4)", "No. of rooms (> 5)", "Eaves (Wall opening)", 
                    "Ceiling presence", "No. of sleepers (4 - 6)", "No. of sleepers (> 7)", "Bushes/grass", "Location (West)", 
                    "Location (Urban)", "Breeding containers")

fore <- ggplot(ab_coeff, aes(y = Est., x = reorder(Coeff, Est.))) + geom_point(size = 15, color = "darkgreen") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.4, size = 4) +
  coord_flip() +
  theme_linedraw() + 
  geom_hline(yintercept = 1, linetype = 1, color = "black", size = 4) +
  labs(title = "Risk Factors for Pupal Abundance") +
  xlab("Risk Factor") +
  ylab("Odds Ratio (95% CI)") +
  scale_x_discrete(breaks = ab_coeff$Coeff, labels = ab_coeff$Names) +
  scale_y_continuous(breaks = c(1:4)) +
  theme(plot.title = element_text(hjust = 1, size = 76),
        legend.title = element_blank(), 
        axis.text = element_text(size = 58),
        axis.title.x = element_text(size = 56, margin = ),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.y = element_text(size = 68, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_line(size = 0.01, color = 'gray65'),
        plot.margin = margin(0.30, 0.30, 0.30, 0.80, "cm"), #t, r, b, l
        legend.position = "none", 
        panel.border = element_rect(colour = "black", fill = NA, size = 1.1) 
        )
  

setEPS()
postscript("Plots/Abundance. Forest.eps",  width = 20, height = 25)
plot(fore)
dev.off()
#********************************************************************************#

#********************************************************************************#
#Full Abundance Model.
pers_coeff <- read.csv('Model Results/Persistence. Full. Coefficients.csv')

pers_coeff$Names <- c("Wall material (Cement)", "Roof material (Grass)", "Roof material (Other)", "Firewood use", 
                     "No. of rooms (3 - 4)", "No. of rooms (> 4)", "Eaves (Wall opening)", 
                    "Ceiling presence", "No. of sleepers (4 - 6)", "No. of sleepers (> 6)", "Bushes/grass", 
                    "Breeding containers")

pers_fore <- ggplot(pers_coeff, aes(y = Est., x = reorder(Coeff, Est.))) + geom_point(size = 15, color = "darkgreen") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.4, size = 4) +
  coord_flip() +
  theme_linedraw() + 
  geom_hline(yintercept = 1, linetype = 1, color = "black", size = 4) +
  labs(title = "Risk Factors for Pupal Persistence") +
  xlab("Risk Factors") +
  ylab("Odds Ratio (95% CI)") +
  scale_x_discrete(breaks = pers_coeff$Coeff, labels = pers_coeff$Names) +
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14), limits = c(0, 14)) +
  theme(plot.title = element_text(hjust = 1, size = 76),
        legend.title = element_blank(), 
        axis.text = element_text(size = 58),
        axis.title.x = element_text(size = 56),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.y = element_text(size = 68, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
        legend.position = "none", 
        panel.border = element_rect(colour = "black", fill = NA, size = 1.1))


setEPS()
postscript("Plots/Persistence. Forest.eps",  width = 20, height = 25)
plot(pers_fore)
dev.off()
