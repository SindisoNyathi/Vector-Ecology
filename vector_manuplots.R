#********************************************************************************#
#02/09/2019
#Sindiso Nyathi
#Goal: Data visualization for Vector Data.
#********************************************************************************#

#********************************************************************************#
#Load packages, set directories and read in required files.
#Set working directory, read in files and set libraries.
setwd()

#Load the required libraries
these_packages <- c("ggplot2", "ggthemes", "tidyverse", "naniar", "magrittr", "lubridate", 
                    "ggpubr", "gridExtra", "grid")

lapply(these_packages, require, character.only = TRUE)

#Read in the files we are merging.
lvpk <- readRDS("Data Files/Abundance Data.rds")
lvpk_tot <- read.csv("Data Files/Overall Totals.csv")
#********************************************************************************#

#********************************************************************************#
#Some preliminary work with dates and what not. 
kis <- lvpk[lvpk$Site == "Kisumu",]
chu <- lvpk[lvpk$Site == "Chulaimbo",]
msa <- lvpk[lvpk$Site == "Msambweni",]
uku <- lvpk[lvpk$Site == "Ukunda",]

kis_tot <- lvpk_tot[lvpk_tot$Site == "Kisumu",]
chu_tot <- lvpk_tot[lvpk_tot$Site == "Chulaimbo",]
msa_tot <- lvpk_tot[lvpk_tot$Site == "Msambweni",]
uku_tot <- lvpk_tot[lvpk_tot$Site == "Ukunda",]

#Group the elements for ease of use.
sitenames <- list("Kisumu", "Ukunda", "Chulaimbo", "Msambweni")
sites <- list(kis, uku, chu, msa)
sites_tot <- list(kis_tot, uku_tot, chu_tot, msa_tot)


#********************************************************************************#

#********************************************************************************#
#Plot 1: Pupae Counts, Bar Plots.
{
  pupae_bar_kis <- ggplot(kis_tot, aes(x = reorder(HouseID, -total_pupaecounts), y = total_pupaecounts)) +
    geom_bar(stat = "identity", fill = "slategray3", na.rm = TRUE) +
    theme_bw() +
    ggtitle("Kisumu") +
    xlab("") +
    ylab("") +
    ylim(0, 6000) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text = element_text(size=14),
          plot.title = element_text(size = 16), 
          axis.title.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18))
  
  pupae_bar_chu <- ggplot(chu_tot, aes(x = reorder(HouseID, -total_pupaecounts), y = total_pupaecounts)) +
    geom_bar(stat = "identity", fill = "slategray3", na.rm = TRUE) +
    theme_bw() +
    ggtitle("Chulaimbo") +
    xlab("") +
    ylab("") +
    ylim(0, 6000) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text = element_text(size=14),
          plot.title = element_text(size = 16), 
          axis.title.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18))
  
  pupae_bar_uku <- ggplot(uku_tot, aes(x = reorder(HouseID, -total_pupaecounts), y = total_pupaecounts)) +
    geom_bar(stat = "identity", fill = "slategray3", na.rm = TRUE) +
    theme_bw() +
    ggtitle("Ukunda") +
    xlab("") +
    ylab("") +
    ylim(0, 6000) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text = element_text(size=14),
          plot.title = element_text(size = 16), 
          axis.title.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18))
  
  pupae_bar_msa <- ggplot(msa_tot, aes(x = reorder(HouseID, -total_pupaecounts), y = total_pupaecounts)) +
    geom_bar(stat = "identity", fill = "slategray3", na.rm = TRUE) +
    theme_bw() +
    ggtitle("Msambweni") +
    xlab("") +
    ylab("") +
    ylim(0, 6000) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text = element_text(size=14),
          plot.title = element_text(size = 16), 
          axis.title.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18))
  
  setEPS()
  postscript("Plots/Pupae Counts.eps", width = 15, height = 8)
  grid.arrange(pupae_bar_kis,pupae_bar_chu, pupae_bar_uku, pupae_bar_msa,
               nrow = 2, ncol = 2, top = textGrob("Total Pupae Counts", gp=gpar(fontsize=20)),
               left = textGrob("Pupae Counts", rot = 90, gp=gpar(fontsize=18)), 
               bottom = textGrob("HouseID", gp=gpar(fontsize=18)))
  dev.off()
  
}  

#Plot 2: Positive Months, Bar Plots.
{
  pupae_bar_kis2 <- ggplot(kis_tot, aes(x = reorder(HouseID, -MonthCount), y = MonthCount)) +
    geom_bar(stat = "identity", fill = "slategray3", na.rm = TRUE) +
    theme_bw() +
    ggtitle("Kisumu") +
    xlab("") +
    ylab("") +
    ylim(0, 54) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text = element_text(size=14),
          plot.title = element_text(size = 16), 
          axis.title.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18)) + 
    geom_hline(yintercept = 48, linetype = 2, color = "black", size = 0.25) + 
    annotate(geom = "text", x = -0.17, y = 48, label = "48", colour = "gray30", size = 5) +
    coord_cartesian(xlim = c(1, 20), clip = 'off')
  
  pupae_bar_chu2 <- ggplot(chu_tot, aes(x = reorder(HouseID, -MonthCount), y = MonthCount)) +
    geom_bar(stat = "identity", fill = "slategray3", na.rm = TRUE) +
    theme_bw() +
    ggtitle("Chulaimbo") +
    xlab("") +
    ylab("") +
    ylim(0, 54) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text = element_text(size=14),
          plot.title = element_text(size = 16), 
          axis.title.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18)) + 
    geom_hline(yintercept = 48, linetype = 2, color = "black", size = 0.25) + 
    annotate(geom = "text", x = -0.17, y = 48, label = "48", colour = "gray30", size = 5) +
    coord_cartesian(xlim = c(1, 20), clip = 'off')
  
  pupae_bar_uku2 <- ggplot(uku_tot, aes(x = reorder(HouseID, -MonthCount), y = MonthCount)) +
    geom_bar(stat = "identity", fill = "slategray3", na.rm = TRUE) +
    theme_bw() +
    ggtitle("Ukunda") +
    xlab("") +
    ylab("") +
    ylim(0, 54) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text = element_text(size=14),
          plot.title = element_text(size = 16), 
          axis.title.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18)) + 
    geom_hline(yintercept = 48, linetype = 2, color = "black", size = 0.25) + 
    annotate(geom = "text", x = -0.17, y = 48, label = "48", colour = "gray30", size = 5) +
    coord_cartesian(xlim = c(1, 20), clip = 'off')
  
  pupae_bar_msa2 <- ggplot(msa_tot, aes(x = reorder(HouseID, -MonthCount), y = MonthCount)) +
    geom_bar(stat = "identity", fill = "slategray3", na.rm = TRUE) +
    theme_bw() +
    ggtitle("Msambweni") +
    xlab("") +
    ylab("") +
    ylim(0, 54) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text = element_text(size=14),
          plot.title = element_text(size = 16), 
          axis.title.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18)) + 
    geom_hline(yintercept = 48, linetype = 2, color = "black", size = 0.25) + 
    annotate(geom = "text", x = -0.17, y = 48, label = "48", colour = "gray30", size = 5) +
    coord_cartesian(xlim = c(1, 28), clip = 'off')
  
  
  setEPS()
  postscript("Plots/Presence Months.eps",  width = 15, height = 8)
  grid.arrange(pupae_bar_kis2,pupae_bar_chu2, pupae_bar_uku2, pupae_bar_msa2,
               nrow = 2, ncol = 2, top = textGrob("Total Presence Months", gp=gpar(fontsize=20)),
               left = textGrob("No. Presence Months", rot = 90, gp=gpar(fontsize=18)), 
               bottom = textGrob("HouseID", gp=gpar(fontsize=18)))
  dev.off()
  
}

#Plot 3: The raster plots.
plot_raster <- function(site, sitename){
  
  #Diagnostic
  #site <- sites[[1]]
  #Make NA zero for now. 
  site$Pupae[is.na(site$Pupae)] <- 0
  site$MonLa <- substr(site$Month, 1, 1)
  
  pupae_raster <- ggplot(site, aes(x = site$MnYr, y = reorder(HouseID, Pupae), fill =  site$Pupae)) +
    geom_raster() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.text = element_text(size = 10),
          plot.title = element_text(size = 20), 
          axis.title.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18), 
          legend.text = element_text(size=14), 
          legend.title = element_text(size=16), 
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA)) +
    scale_x_discrete(breaks = site$MnYr, labels = site$Month) + 
    ggtitle(sitename)  +
    xlab("") +
    ylab("House ID.") +
    theme(plot.title = element_text(hjust = 0.5)) +
   # scale_fill_gradient(name = "Pupae \nCount", low = "lightslategrey", high = "royalblue4", 
    #                   limits = c(0, 250), breaks = c(50, 100, 150, 200),
     #                 na.value = "gray15") +
    geom_vline(xintercept = 12.5, linetype = 1, color = "white", size = 0.5) +
    geom_vline(xintercept = 24.5, linetype = 1, color = "white", size = 0.5) +
    geom_vline(xintercept = 36.5, linetype = 1, color = "white", size = 0.5) +
    geom_vline(xintercept = 48.5, linetype = 1, color = "white", size = 0.5) +
    annotate(geom = "text", x = 6.5, y = -0.75, label = "2014 - 15" , color = "black", size = 5) +
    annotate(geom = "text", x = 18.5, y = -0.75, label = "2015 - 16" , color = "black", size = 5) +
    annotate(geom = "text", x = 30.5, y = -0.75, label = "2016 - 17" , color = "black", size = 5) +
    annotate(geom = "text", x = 42.5, y = -0.75, label = "2017 - 18" , color = "black", size = 5) + 
    coord_cartesian(ylim = c(1, 28), expand = TRUE, clip = "off") + 
    scale_fill_gradientn(name = "Pupae \nCount", colours = c("grey95", "royalblue4", "royalblue4"), values = c(0, 1, 250),
                        limits = c(0, 250), breaks = c(0, 100, 200), na.value = "gray15", 
                         guide = "colourbar")
    #Chulaimbo
    # geom_text(x = 49.25, y = 19.75, label = "*", size = 12)  +
    # geom_text(x = 49.25, y = 18.75, label = "*", size = 12) +
    # geom_text(x = 49.25, y = 17.75, label = "*", size = 12) +
    # geom_text(x = 49.25, y = 16.75, label = "*", size = 12) +
    # geom_text(x = 49.25, y = 15.75, label = "*", size = 12) +
    # geom_text(x = 49.25, y = 13.75, label = "*", size = 12) +
    # geom_text(x = 49.25, y = 12.75, label = "*", size = 12)
    
    #Kisumu
    # geom_text(x = 49.25, y = 19.75, label = "*", size = 12) +
    # geom_text(x = 49.25, y = 18.75, label = "*", size = 12) +
    # geom_text(x = 49.25, y = 14.75, label = "*", size = 12) +
    # geom_text(x = 49.25, y = 16.75, label = "*", size = 12) +
    # geom_text(x = 49.25, y = 15.75, label = "*", size = 12) +
    # geom_text(x = 49.25, y = 13.75, label = "*", size = 12) +
    # geom_text(x = 49.25, y = 12.75, label = "*", size = 12) +
    # geom_text(x = 49.25, y = 7.75, label = "*", size = 12)
  
    #Ukunda.
    # geom_text(x = 49.25, y = 19.75, label = "*", size = 12) +
    # geom_text(x = 49.25, y = 17.75, label = "*", size = 12)
  
  #Msambweni
  # geom_text(x = 49.25, y = 19.75, label = "*", size = 12) +
  # geom_text(x = 49.25, y = 17.75, label = "*", size = 12)
  
  jpeg(paste("Plots/Poster. Raster Plots. ", sitename, ".jpg", sep = ""), width = 1000, height = 700)
  plot(pupae_raster)
  dev.off()
}
purrr::map2(sites, sitenames, plot_raster)
plot_raster(sites[[3]], "Chulaimbo")
plot_raster(sites[[1]], "Kisumu")
plot_raster(sites[[2]], "Ukunda")
plot_raster(sites[[4]], "Msambweni")


  
  
  
