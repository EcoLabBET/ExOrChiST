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

## Create a figure for CWE in Top 1% orchid hotspots ------

 # Load the data for British top 1% hotspots of CWE (hectads)
   britain_top <- read.csv("./british_top_hotspots_cwe.csv")

 # Load the data for Irish top 1% hotspots of CWE (hectads)
   ireland_top <- read.csv("./irish_top_hotspots_cwe.csv")
   
# Figure for Ireland separately
   
   IR_plot_all <- 
   ggplot(ireland_top, aes(x = TimePeriod, y = CWE, fill = TimePeriod)) +
   stat_boxplot(geom = "errorbar", width = 0.08, aes(color = TimePeriod), linewidth = 0.3, show.legend = FALSE) +
   geom_boxplot(show.legend = FALSE, width = 0.4, position = position_dodge(width = 0.25), 
                 aes(color = TimePeriod), linewidth = 0.4) +
   scale_fill_manual(values=c("#aadce0", "#e76254", "#376795", "#f7aa58")) + # keep the pallette same throughout paper
     scale_colour_manual(values = c("#3fa5ad", "#a62517", "#213e59", "#c06709")) +
     labs(title = "Ireland", x = "Time Period", y = "Weighted Endemism (CWE)") +
   coord_cartesian(ylim = c(0, 1)) + 
   scale_y_continuous(breaks = seq(0, 1, by = 0.25)) +
   scale_x_discrete(labels = c("1930-69", "1987-99", "2000-09", "2010-19")) +
   theme_minimal() +
   theme(
     axis.text = element_text(family = "Roboto", size = 11, color = "black", vjust = 0.5, hjust = 0.5),
     axis.title.x = element_text(family = "Roboto", size = 13, face = "bold", vjust = 1, margin = margin(t = 12)),  
     axis.title.y = element_text(family = "Roboto", size = 12.5, face = "bold", vjust = 1, margin = margin(r = 12)),
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
     ggplot(britain_top, aes(x = TimePeriod, y = CWE, fill = TimePeriod)) +
     stat_boxplot(geom = "errorbar", width = 0.08, aes(color = TimePeriod), linewidth = 0.3, show.legend = FALSE) +
     geom_boxplot(show.legend = FALSE, width = 0.4, position = position_dodge(width = 0.25), 
                  aes(color = TimePeriod), linewidth = 0.4) +
     scale_fill_manual(values=c("#aadce0", "#e76254", "#376795", "#f7aa58")) + # keep the pallette same throughout paper
     scale_colour_manual(values = c("#3fa5ad", "#a62517", "#213e59", "#c06709")) +
     labs(title = "Great Britain", x = "Time Period", y = "Weighted Endemism (CWE)") +
     coord_cartesian(ylim = c(0, 1.2)) + 
     scale_y_continuous(breaks = seq(0, 1.2, by = 0.3)) +
     scale_x_discrete(labels = c("1930-69", "1987-99", "2000-09", "2010-19")) +
     theme_minimal() +
     theme(
       axis.text = element_text(family = "Roboto", size = 11, color = "black", vjust = 0.5, hjust = 0.5),
       axis.title.x = element_text(family = "Roboto", size = 13, face = "bold", vjust = 1, margin = margin(t = 12)),  
       axis.title.y = element_text(family = "Roboto", size = 12.5, face = "bold", vjust = 1, margin = margin(r = 12)), 
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
   GB_IR_top1 <- grid.arrange(IR_plot_all, GB_plot_all, nrow = 2, ncol = 1)
   
# Save all figures as TIFF files
   
   ggsave("./ireland_top_hotspots_cwe_final.tiff", IR_plot_all, width = 12, height = 10, units = "cm", dpi = 300)
   ggsave("./britain_top_hotspots_cwe_final.tiff", GB_plot_all, width = 12, height = 10, units = "cm", dpi = 300)
   ggsave("./GB_IR_top_hotspots_cwe_final.tiff", GB_IR_top1, width = 12, height = 24, units = "cm", dpi = 400)
   ggsave("./GB_IR_top_hotspots_cwe_final.jpeg", GB_IR_top1, width = 12, height = 24, units = "cm", dpi = 300)
   