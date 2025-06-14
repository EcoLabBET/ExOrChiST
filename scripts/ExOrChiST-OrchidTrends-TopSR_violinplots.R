## Changes in Taxonomic Diversity of British and Irish orchid hotspots across time ----

setwd("D:/1_ExOrChiST-data/ExOrChiST-GDrive folder/Plant_Atlas_Trends-WORK WITH THESE/Hotspot_Analysis")

# Load required libraries
library(ggplot2)
library(devtools)
library(dplyr)
library(MetBrewer)
library(extrafont)
library(extrafontdb)
library(gridExtra)

font_import()

## Create a figure for SR in Top 1% taxonomic diversity hotspots ------

 # Load the data for British top 1% hotspots of taxonomic diversity (hectads)
   britain_top <- read.csv("./british_top_hotspots_new.csv")

 # Load the data for Irish top 1% hotspots of taxonomic diversity (hectads)
   ireland_top <- read.csv("./irish_top_hotspots.csv")
   
# Figure for Ireland separately
   
   IR_plot_all <- 
   ggplot(ireland_top, aes(x = TimePeriod, y = SR, fill = TimePeriod)) +
   stat_boxplot(geom = "errorbar", width = 0.08, aes(color = TimePeriod), linewidth = 0.3, show.legend = FALSE) +
   geom_boxplot(show.legend = FALSE, width = 0.4, position = position_dodge(width = 0.25), 
                 aes(color = TimePeriod), linewidth = 0.4) +
   scale_fill_manual(values=c("#aadce0", "#e76254", "#376795", "#f7aa58")) + # keep the pallette same throughout paper
     scale_colour_manual(values = c("#3fa5ad", "#a62517", "#213e59", "#c06709")) +
     labs(title = "Ireland", x = "Time Period", y = "Species Richness") +
   coord_cartesian(ylim = c(0, 25)) + 
   scale_y_continuous(breaks = seq(0, 25, by = 10)) +
   scale_x_discrete(labels = c("1930-69", "1987-99", "2000-09", "2010-19")) +
   theme_minimal() +
   theme(
     axis.text = element_text(family = "Roboto", size = 11, color = "black", vjust = 0.5, hjust = 0.5),
     axis.title.x = element_text(family = "Roboto", size = 13, face = "bold", vjust = 1, margin = margin(t = 12)),  
     axis.title.y = element_text(family = "Roboto", size = 13, face = "bold", vjust = 1, margin = margin(r = 12)),
     plot.title = element_text(family = "Roboto", size = 14, face = "bold", colour = "black",hjust = 1),
     plot.background = element_rect(fill = "white", colour = NA),
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4),
     axis.ticks = element_line(linewidth = 0.3),
     axis.line = element_line(linewidth = 0.3)
   ) 
   
   print(IR_plot_all)
   
   # Figure for Britain separately
   
   GB_plot_all <- 
     ggplot(britain_top, aes(x = TimePeriod, y = SR, fill = TimePeriod)) +
     stat_boxplot(geom = "errorbar", width = 0.08, aes(color = TimePeriod), linewidth = 0.3, show.legend = FALSE) +
     geom_boxplot(show.legend = FALSE, width = 0.4, position = position_dodge(width = 0.25), 
                  aes(color = TimePeriod), linewidth = 0.4) +
     scale_fill_manual(values=c("#aadce0", "#e76254", "#376795", "#f7aa58")) + # keep the pallette same throughout paper
     scale_colour_manual(values = c("#3fa5ad", "#a62517", "#213e59", "#c06709")) +
     labs(title = "Great Britain", x = "Time Period", y = "Species Richness") +
     coord_cartesian(ylim = c(0, 25)) + 
     scale_y_continuous(breaks = seq(0, 25, by = 10)) +
     scale_x_discrete(labels = c("1930-69", "1987-99", "2000-09", "2010-19")) +
     theme_minimal() +
     theme(
       axis.text = element_text(family = "Roboto", size = 11, color = "black", vjust = 0.5, hjust = 0.5),
       axis.title.x = element_text(family = "Roboto", size = 13, face = "bold", vjust = 1, margin = margin(t = 12)),  
       axis.title.y = element_text(family = "Roboto", size = 13, face = "bold", vjust = 1, margin = margin(r = 12)), 
       plot.title = element_text(family = "Roboto", size = 14, face = "bold", colour = "black", hjust = 1),
       plot.background = element_rect(fill = "white", colour = NA),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4),
       axis.ticks = element_line(linewidth = 0.3),
       axis.line = element_line(linewidth = 0.3)
     ) 
   
   print(GB_plot_all)
   
   
# Two-paneled figure
   GB_IR_top1 <- grid.arrange(IR_plot_all, GB_plot_all, nrow = 1, ncol = 2)
   
# Save all figures as TIFF files
   
   ggsave("./ireland_top_hotspots_final.tiff", IR_plot_all, width = 12, height = 10, units = "cm", dpi = 300)
   ggsave("./britain_top_hotspots_final.tiff", GB_plot_all, width = 12, height = 10, units = "cm", dpi = 300)
   ggsave("./GB_IR_top_hotspots_final.tiff", GB_IR_top1, width = 20, height = 18, units = "cm", dpi = 400)
   ggsave("./GB_IR_top_hotspots_final.jpeg", GB_IR_top1, width = 20, height = 18, units = "cm", dpi = 300)
   