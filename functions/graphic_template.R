####
# Project: Movement during pregnancy and health
# Purpose: Create the monthly data
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 2025/09/20
####

library(ggplot2)


theme_set(theme_bw(base_size=12, base_family = "serif"))

theme_update(
    strip.background=element_rect(fill="white"),
    strip.text=element_text(face="bold"),
    axis.text = element_text(colour="black"),
    axis.title = element_text(face="bold"),
    panel.grid.major=element_line(colour="grey", size=0.25, linetype = "dotted"),
    panel.grid.minor=element_blank(),
    legend.position="bottom", 
    panel.spacing = unit(0.4, "cm"),
    plot.margin  = margin(t=0.2, r=0.5, b=0.2, l=0.5, "cm")
)
