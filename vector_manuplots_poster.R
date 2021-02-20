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
lvpk <- readRDS("Data Files/Vector Abundance Dataset.rds")
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
sitenames <- list("Kisumu (Urban, Inland)", "Ukunda (Urban, Coast)", "Chulaimbo (Rural, Inland)", "Msambweni (Rural, Coast)")
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

plot_raster2 <- function(site, sitename){
  
  #Diagnostic.
  #site <- sites[[2]]
  #site_tot <- sites_tot[[2]]
  #sitename <- "Ukunda"  
  
  #Diagnostic.
  #site <- sites
  site$Pupae[is.na(site$Pupae)] <- 0
  
  pupae_raster <- ggplot(site, aes(x = site$MnYr, y = reorder(HouseID, Pupae), fill =  site$Pupae)) +
    geom_raster() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 16), 
          axis.text.y = element_text(size = 20),
          plot.title = element_text(size = 32), 
          axis.title.x = element_text(size = 24), 
          axis.title.y = element_text(size = 24), 
          legend.text = element_text(size = 18), 
          legend.title = element_text(size = 20),
          legend.position = c(1.12, 1.16),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA)) +
    scale_x_discrete(breaks = site$MnYr, labels = site$Month) + 
    #ggtitle(sitename)  +
    xlab("") +
    ylab("House ID.") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_gradient(name = "Pupae Count", low = "mintcream", high = "darkgreen", 
                        limits = c(0, 50), breaks = c(0, 25, 50),
                        na.value = "gray15") +
    geom_vline(xintercept = 12.5, linetype = 1, color = "white", size = 0.5) +
    geom_vline(xintercept = 24.5, linetype = 1, color = "white", size = 0.5) +
    geom_vline(xintercept = 36.5, linetype = 1, color = "white", size = 0.5) +
    geom_vline(xintercept = 48.5, linetype = 1, color = "white", size = 0.5) +
    annotate(geom = "text", x = 6.5, y = -1.7, label = "2014 - 15" , color = "black", size = 8) +
    annotate(geom = "text", x = 18.5, y = -1.7, label = "2015 - 16" , color = "black", size = 8) +
    annotate(geom = "text", x = 30.5, y = -1.7, label = "2016 - 17" , color = "black", size = 8) +
    annotate(geom = "text", x = 42.5, y = -1.7, label = "2017 - 18" , color = "black", size = 8) + 
    coord_cartesian(ylim = c(1, 20), expand = T, clip = "off")
  
  
  #Hist for totals.
  site_hou <- aggregate(site$Pupae, by = list(site$HouseID), FUN = sum, na.rm = T)
  colnames(site_hou) <- c('HouseID', 'Pupae')
  pupae_bar <- ggplot(site_hou, aes(x = reorder(HouseID, Pupae), y = Pupae)) +
    geom_bar(stat = "identity", fill = "forestgreen", na.rm = TRUE) +
    theme_bw() +
    xlab("") +
    ylab("Total Pupae Count") +
    scale_y_continuous(position = "left", limits = c(0, 750)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 18), 
          axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14), 
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.background = element_rect(fill="white", color = "white"),
          #panel.grid.minor.x = element_line(), 
          #panel.grid.major.x = element_blank(), 
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(), 
          plot.margin = unit(c(0.25, 1.5, 0.9, -0.5), "cm")) + coord_flip()
  
  #Hist for totals. 
  #aggregate site by month year. 
  site_mnyr <- aggregate(site$Pupae, by = list(site$MnYr), FUN = sum, na.rm = T)
  colnames(site_mnyr) <- c('MnYr', 'Pupae')
  pupae_bar_month <- ggplot(site_mnyr, aes(x = MnYr, y = Pupae)) +
    geom_bar(stat = "identity", fill = "forestgreen", na.rm = TRUE) +
    theme_bw() +
    xlab("") +
    ylab("Total Pupae Count") +
    #scale_x_discrete(drop=F) + 
    scale_y_continuous(position = "left", limits = c(0, 350)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(
      #axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5),
      axis.text = element_text(size = 12),
      plot.title = element_text(size = 18), 
      axis.title.x = element_text(size = 14),
      plot.background = element_rect(fill="white", color = "white"),
      axis.title.y = element_text(size = 14), 
      panel.grid.minor.y = element_blank(),
      #panel.grid.major.y = element_blank(),
      #panel.grid.minor.x = element_line(), 
      panel.grid.major.x = element_blank(), 
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(), 
      plot.margin = unit(c(0.5, 0.17, -0.5, 1.4), "cm")
    )
  
  #plot(pupae_bar)
  
  blank <- grid.rect(gp=gpar(col="white"))
  
  jpeg(paste("Plots/Raster Plots. ", sitename, ".jpg", sep = ""), width = 1000, height = 750)
  grid.arrange(pupae_bar_month, blank,  pupae_raster, pupae_bar,  ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4), 
               top = grid::textGrob(sitename, gp = grid::gpar(fontsize = 32)), 
               newpage = T)
  dev.off()
  
  #return(final_raster)
}

purrr::map2(sites, sitenames, plot_raster2)

plot_raster(sites[[3]], sitenames[[3]])
plot_raster(sites[[3]], "Chulaimbo")
plot_raster(sites[[1]], "Kisumu")
plot_raster(sites[[2]], "Ukunda")
plot_raster(sites[[4]], "Msambweni")


#Plot 3: The raster plots.
plot_raster <- function(site, sitename){
  
  #Diagnostic
  #site <- sites[[1]]
  #Make NA zero for now. 
  site$Pupae[is.na(site$Pupae)] <- 0
  
  pupae_raster <- ggplot(site, aes(x = site$MnYr, y = reorder(HouseID, Pupae), fill =  site$Pupae)) +
    geom_raster() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.text = element_text(size = 20),
          plot.title = element_text(size = 32), 
          axis.title.x = element_text(size = 24), 
          axis.title.y = element_text(size = 24), 
          legend.text = element_text(size = 20), 
          legend.title = element_text(size = 24), 
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA)) +
    scale_x_discrete(breaks = site$MnYr, labels = site$Month) + 
    ggtitle(sitename)  +
    xlab("") +
    ylab("House ID.") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_gradient(name = "Pupae \nCount", low = "mintcream", high = "darkgreen", 
                        limits = c(0, 50), breaks = c(0, 25, 50),
                        na.value = "gray15") +
    geom_vline(xintercept = 12.5, linetype = 1, color = "white", size = 0.5) +
    geom_vline(xintercept = 24.5, linetype = 1, color = "white", size = 0.5) +
    geom_vline(xintercept = 36.5, linetype = 1, color = "white", size = 0.5) +
    geom_vline(xintercept = 48.5, linetype = 1, color = "white", size = 0.5) +
    annotate(geom = "text", x = 6.5, y = -1, label = "2014 - 15" , color = "black", size = 8) +
    annotate(geom = "text", x = 18.5, y = -1, label = "2015 - 16" , color = "black", size = 8) +
    annotate(geom = "text", x = 30.5, y = -1, label = "2016 - 17" , color = "black", size = 8) +
    annotate(geom = "text", x = 42.5, y = -1, label = "2017 - 18" , color = "black", size = 8) + 
    coord_cartesian(ylim = c(1, 20), expand = T, clip = "off") #+ 
  #scale_fill_gradientn(name = "Pupae \nCount", colours = c("grey95", "royalblue4", "royalblue4"), values = c(0, 1, 250),
  #                   limits = c(0, 50), breaks = c(0, 25, 50), na.value = "gray15", 
  #                   guide = "colourbar")
  plot(pupae_raster)
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


  
  
  
