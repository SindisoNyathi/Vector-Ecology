#********************************************************************************#
#12/22/2019
#Sindiso Nyathi
#Goal: GAMM Model with R2BayesX
#********************************************************************************#

#********************************************************************************#
# #Preliminaries.
#Set the working directory.
setwd()

#Load the required libraries. This may require you to install some of these. 
these_packages <- c("naniar", "magrittr", "lubridate", "ggpubr", 
                    "writexl", "reshape2", "synchrony",  "R2BayesX", 
                    "itsadug", "MASS", "rgdal", "raster", "sf", "BayesX", 
                    "ggplot2", "ggpubr", "gridExtra")

#Load the required packages.
lapply(these_packages, require, character.only = TRUE)

#Read in the models. 
#Full. 
ab_full_trm <- readRDS("Model Results/Abundance Model. Full. Mult. TRM.rds")
ab_wes_trm <- readRDS("Model Results/Abundance Model. West. Mult. TRM.rds")
ab_coa_trm <- readRDS("Model Results/Abundance Model. Coast. Mult. TRM.rds")
pers_full <- readRDS("Model Results/Persistence Model. Full.rds")

#Sensitivity Analysis Plots.
replacements_sa <- readRDS("Model Results/Abundance Model. Full. Mult. SA.rds")
outliers_sa <- readRDS("Model Results/Abundance Model. Mult. SA. Outliers.rds")
#****************************************************************************************#
#****************************************************************************************#
#Month Plots
#Function. 
effect_plots_month <- function(data, this_label) {
  
  month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
             "Sep", "Oct", "Nov", "Dec")
  
  effect_plot <- ggplot(data, aes(x = data[,1], y = exp(data[,2]))) +
    geom_line() + 
    geom_ribbon(aes(ymin = exp(data[,3]), ymax = exp(data[,7])), alpha=0.2) + 
    labs(title = this_label) +
    xlab("Month") +
    ylab("Odds Ratio (95% CI)") +
    ylim(c(0, 2.5)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 38),
          legend.title = element_blank(), 
          axis.text = element_text(size = 26),
          axis.title.x = element_text(size = 30),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.y = element_text(size = 30), 
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
          legend.position = "none") + 
    geom_hline(yintercept = 1, linetype = 1, color = "black", size = 0.5) +
    geom_vline(xintercept = 3.5, linetype = 2, color = "black", size = 0.25) +
    geom_vline(xintercept = 6.5, linetype = 2, color = "black", size = 0.25) +
    geom_vline(xintercept = 9.5, linetype = 2, color = "black", size = 0.25) +
    annotate(geom = "text", x= 2, y = 0.2, label = "Long Dry", color = "black", size = 12) +
    annotate(geom = "text", x= 5, y = 0.2, label = "Long Rain", color = "black", size = 12) +
    annotate(geom = "text", x= 8, y = 0.2, label = "Short Dry", color = "black", size = 12) +
    annotate(geom = "text", x= 11, y = 0.2, label = "Short Rain", color = "black", size = 12) +
    scale_x_continuous(breaks = data[,1], labels = month)
  
  return(effect_plot)
  
}

#Abundance full. Month
data_abu_full_month <- ab_full_trm$effects$`sx(MonthNum)`
plot_abu_full_month <- effect_plots_month(data_abu_full_month, "Overall Model")
plot(plot_abu_full_month)

#Abundance full. Month-West
data_abu_full_month_wes <- ab_wes_trm$effects$`sx(MonthNum)`
plot_abu_full_month_wes <- effect_plots_month(data_abu_full_month_wes, "Western Households Model")

#Abundance full. Month-Coast
data_abu_full_month_coa <- ab_coa_trm$effects$`sx(MonthNum)`
plot_abu_full_month_coa <- effect_plots_month(data_abu_full_month_coa, "Coastal Households Model")

#Save the main one. 
jpeg("Plots/Month Effect.jpg",  width = 1000, height = 750)
plot(plot_abu_full_month)
dev.off()

#Arrange.
jpeg("Plots/All. Month Effect.jpg",  width = 2000, height = 1500)
#plot(plot_abu_full_month)
 grid.arrange(plot_abu_full_month, plot_abu_full_month_coa, plot_abu_full_month_wes,
              widths = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), 
              layout_matrix = rbind(c(NA, NA, 1, 1, 1, 1, 1, 1, NA, NA), c(3, 3, 3, 3, 3, 2, 2, 2, 2, 2)), 
              top = grid::textGrob("Effect of Month on Pupal Abundance", gp = grid::gpar(fontsize = 38)))
dev.off()

#****************************************************************************************#
#****************************************************************************************#
#Year Plots
#Function. 
effect_plots_year <- function(data, this_label) {
  
  effect_plot <- ggplot(data, aes(x = data[,1], y = exp(data[,2]))) +
    geom_line() + 
    geom_point(size = 3) + 
    geom_ribbon(aes(ymin = exp(data[,3]), ymax = exp(data[,7])), alpha = 0.2) + 
    labs(title = this_label) +
    xlab("Year") +
    ylab("Odds Ratio (95% CI)") +
    ylim(c(0, 3)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 38),
          legend.title = element_blank(), 
          axis.text = element_text(size = 26),
          axis.title.x = element_text(size = 30),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.y = element_text(size = 30),
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
          legend.position = "none") + 
    geom_hline(yintercept = 1, linetype = 2, color = "blue", size = 0.5) +
    scale_x_continuous(labels = c("2014 -15", "2015 -16", "2016 - 17", "2017 -18"))
  
  return(effect_plot)
  
}

#Abundance full. Month
data_abu_full_yer <- ab_full_trm$effects$`sx(YearNum)`
plot_abu_full_yer <- effect_plots_year(data_abu_full_yer, "Overall Model")

#Abundance full. Month-West
data_abu_full_yer_wes <- ab_wes_trm$effects$`sx(YearNum)`
plot_abu_full_yer_wes <- effect_plots_year(data_abu_full_yer_wes, "Western Households Model")

#Abundance full. Month-Coast
data_abu_full_yer_coa <- ab_coa_trm$effects$`sx(YearNum)`
plot_abu_full_yer_coa <- effect_plots_year(data_abu_full_yer_coa,  "Coastal Households Model")

#Arrange.
jpeg("Plots/Year Effect.jpg",  width = 1000, height = 750)
plot(plot_abu_full_yer)
dev.off()

#Arrange.
jpeg("Plots/All. Year Effect.jpg",  width = 1500, height = 1000)
grid.arrange(plot_abu_full_yer, plot_abu_full_yer_coa, plot_abu_full_yer_wes,
             widths = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), 
             layout_matrix = rbind(c(NA, NA, 1, 1, 1, 1, 1, 1, NA, NA), c(3, 3, 3, 3, 3, 2, 2, 2, 2, 2)), 
              top = grid::textGrob("Effect of Year on Pupal Abundance", gp = grid::gpar(fontsize = 42)))
dev.off()


#Persistence plots. 
#Persistence full. Year.
data_pers_full_yer <- pers_full$effects$`sx(YearNum)`
plot_pers_full_yer <- effect_plots_year(data_pers_full_yer, "Overall")
jpeg("Plots/Persistence. Year Effect.jpg",  width = 1000, height = 750)
plot(plot_pers_full_yer)
dev.off()
#****************************************************************************************#
#****************************************************************************************#
#Model Diagnostic Plots for the 4 main ones. 
models <- list(per_full, abu_full, abu_wes, abu_coa)

#Residual Plots Function 
residual_plots <- function(this_model, this_label){
  
  #Diagnostic.
  this_model <- abu_coa_trm
  
  fitted <- as.data.frame(fitted(this_model))
  fitted <- fitted[,1][!is.na(fitted[,1])]
  fitted <- exp(fitted)/(1 + exp(fitted))
  
  mod_resp <- as.numeric(this_model$response)
  mod_resp[mod_resp == 1] <- 0
  mod_resp[mod_resp != 0] <- 1
  
  rez <- (mod_resp - fitted)/sqrt(fitted*(1-fitted))
  
  #Make the dataframe
  rez <- as.data.frame(list(rez, fitted))
  
  #summary(rez)
  
  rez$No <- 1:nrow(rez)
  
  #The 4 plots. 
  #Histogram
  hist_plot <- ggplot(rez, aes(x = rez[,1])) + 
    geom_histogram(bins = 50, fill = NA, colour = "black") + 
    theme_linedraw() +
    labs(title = "a) Model Residuals (Histogram)") +
    xlab("Bins") +
    ylab("Density")+
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_blank(), 
          axis.text = element_text(size=12),
          axis.title.x = element_text(size = 14), 
          axis.text.x = element_text(hjust = 1),
          panel.grid.major.x = element_line(size = 0.05),
          panel.grid.minor.x = element_line(size = 0.05),
          panel.grid.major.y = element_line(size = 0.05),
          panel.grid.minor.y = element_line(size = 0.05),
          axis.title.y = element_text(size = 14), 
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
          legend.text = element_text(size = 14))
  
  #Scatter Plot
  scat_plot <- ggplot(rez, aes(y = rez[,1], x = rez[,3])) + 
    geom_point() + 
    theme_linedraw() +
    labs(title = "b) Model Residuals (Scatter Plot)") +
    xlab("Index") +
    ylab("Residuals") +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_blank(), 
          axis.text = element_text(size=12),
          axis.title.x = element_text(size = 14), 
          axis.text.x = element_text(hjust = 1),
          panel.grid.major.x = element_line(size = 0.05),
          panel.grid.minor.x = element_line(size = 0.05),
          panel.grid.major.y = element_line(size = 0.05),
          panel.grid.minor.y = element_line(size = 0.05),
          axis.title.y = element_text(size = 14), 
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
          legend.text = element_text(size = 14))
  
  #QQplot
  qq_plot <- ggplot(rez, aes(sample = rez[,1])) + 
    stat_qq() + stat_qq_line() + 
    theme_linedraw() +
    labs(title = "c) Model Residuals (Q-Q Plot)") +
    xlab("Index") +
    ylab("Residuals") +
    #ylim(c(-100, 100)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_blank(), 
          axis.text = element_text(size=12),
          axis.title.x = element_text(size = 14), 
          axis.text.x = element_text(hjust = 1),
          panel.grid.major.x = element_line(size = 0.05),
          panel.grid.minor.x = element_line(size = 0.05),
          panel.grid.major.y = element_line(size = 0.05),
          panel.grid.minor.y = element_line(size = 0.05),
          axis.title.y = element_text(size = 14), 
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
          legend.text = element_text(size = 14))
  
  #Fitted vs. Residuals
  fitres_plot <- ggplot(rez, aes(y = rez[,1], x = rez[,2])) + 
    geom_point() + 
    theme_linedraw() +
    labs(title = "d) Fitted vs. Residuals") +
    ylab("Residuals") +
    xlab("Fitted") +
    #ylim(c(-100, 100)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_blank(), 
          axis.text = element_text(size=12),
          axis.title.x = element_text(size = 14), 
          axis.text.x = element_text(hjust = 1),
          panel.grid.major.x = element_line(size = 0.05),
          panel.grid.minor.x = element_line(size = 0.05),
          panel.grid.major.y = element_line(size = 0.05),
          panel.grid.minor.y = element_line(size = 0.05),
          axis.title.y = element_text(size = 14), 
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
          legend.text = element_text(size = 14))
  
  jpeg(paste("Plots/(", this_label, ") Model Dx Plots.jpg", sep = ""),  width = 1000, height = 750)
  grid.arrange(hist_plot, scat_plot, qq_plot, fitres_plot, 
               nrow = 2,
               top = grid::textGrob("Model Diagnostic Plots", gp = grid::gpar(fontsize = 22)))
  dev.off()
}


#Call Function
residual_plots(abu_full, "Abundance Overall")
residual_plots(abu_wes, "Abundance West")
residual_plots(abu_coa, "Abundance Coast")
residual_plots(per_full, "Persistence")
#****************************************************************************************#

#****************************************************************************************#
#The Temp and Rainfall plot. 
lvpk <- readRDS("Data Files/Vector Abundance Dataset.rds")

lvpk$MonLa = NA
lvpk$MonLa = ""
lvpk$MonLa[which(lvpk$Month == "Jan")] <- "J"
lvpk$MonLa[which(lvpk$Month == "Feb")] <- "F"
lvpk$MonLa[which(lvpk$Month == "Mar")] <- "M"
lvpk$MonLa[which(lvpk$Month == "Apr")] <- "A"
lvpk$MonLa[which(lvpk$Month == "May")] <- "M"
lvpk$MonLa[which(lvpk$Month == "Jun")] <- "J"
lvpk$MonLa[which(lvpk$Month == "Jul")] <- "J"
lvpk$MonLa[which(lvpk$Month == "Aug")] <- "A"
lvpk$MonLa[which(lvpk$Month == "Sep")] <- "S"
lvpk$MonLa[which(lvpk$Month == "Oct")] <- "O"
lvpk$MonLa[which(lvpk$Month == "Nov")] <- "N"
lvpk$MonLa[which(lvpk$Month == "Dec")] <- "D"

site_kis <- lvpk[lvpk$Site == "Kisumu" & lvpk$HouseID == "11",]
site_uku <- lvpk[lvpk$Site == "Ukunda" & lvpk$HouseID == "1001",]
site_msa <- lvpk[lvpk$Site == "Msambweni" & lvpk$HouseID == "4217",]
site_chu <- lvpk[lvpk$Site == "Chulaimbo" & lvpk$HouseID == "78",]

#Temp
{
  temp_kis <- ggplot(site_kis, aes(x = MnYr)) + 
    geom_bar(aes(group = 1, y = (Rain/17.5), colour = 'Rainfall'), fill = 'dodgerblue', stat = 'identity') +
    geom_line(aes(group = 1, y = Temp, colour = 'Temperature')) +
    scale_y_continuous(sec.axis = sec_axis(~.*17.5), limits = c(0, 35)) +
    scale_colour_manual(values=c('dodgerblue4', "firebrick1")) +
    scale_fill_manual(values=c('dodgerblue', "firebrick1")) +
    theme_bw() +
    labs(title = "Kisumu") +
    ylab("") +
    xlab("") +
    #ylim(c(22, 32)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_blank(), 
          axis.text = element_text(size = 10),
          axis.title.x = element_text(size = 14),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.y = element_text(size = 14), 
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"), 
          legend.text = element_text(size = 18)) + 
    #theme(legend.position = c(1, 1.25)) +
    scale_x_discrete(breaks = site_kis$MnYr, labels = site_kis$MonLa) +
    geom_vline(xintercept = 12.5, linetype = 2, color = "black", size = 0.25) +
    geom_vline(xintercept = 24.5, linetype = 2, color = "black", size = 0.25) +
    geom_vline(xintercept = 36.5, linetype = 2, color = "black", size = 0.25) +
    annotate(geom = "text", x= 6, y = 32.5, label = "2014 - 2015", color = "black", fontface = "bold", size = 5) +
    annotate(geom = "text", x= 18, y = 32.5, label = "2015 - 2016", color = "black", fontface = "bold", size = 5) +
    annotate(geom = "text", x= 30, y = 32.5, label = "2016 - 2017", color = "black", fontface = "bold", size = 5) +
    annotate(geom = "text", x= 42, y = 32.5, label = "2017 - 2018", color = "black", fontface = "bold", size = 5)
 
   temp_uku <- ggplot(site_uku, aes(x = MnYr)) + 
     geom_bar(aes(group = 1, y = (Rain/17.5), colour = 'Rainfall'), fill = 'dodgerblue', stat = 'identity') +
     geom_line(aes(group = 1, y = Temp, colour = 'Temperature')) +
     scale_y_continuous(sec.axis = sec_axis(~.*17.5), limits = c(0, 35)) +
     scale_colour_manual(values=c('dodgerblue4', "firebrick1")) +
     scale_fill_manual(values=c('dodgerblue', "firebrick1")) +
    theme_bw() +
     labs(title = "Ukunda") +
    xlab("") +
    ylab("") +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_blank(), 
          axis.text = element_text(size = 10),
          axis.title.x = element_text(size = 14),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.y = element_text(size = 14), 
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"), 
          legend.position = "bottom", 
          legend.box.background = element_rect(colour = "black")) + 
    scale_x_discrete(breaks = site_uku$MnYr, labels = site_uku$MonLa) +
    geom_vline(xintercept = 12.5, linetype = 2, color = "black", size = 0.25) +
    geom_vline(xintercept = 24.5, linetype = 2, color = "black", size = 0.25) +
    geom_vline(xintercept = 36.5, linetype = 2, color = "black", size = 0.25)
  
  temp_msa <- ggplot(site_msa, aes(x = MnYr)) +
    geom_bar(aes(group = 1, y = (Rain/17.5), colour = 'Rainfall'), fill = 'dodgerblue', stat = 'identity') +
    geom_line(aes(group = 1, y = Temp, colour = 'Temperature')) +
    scale_y_continuous(sec.axis = sec_axis(~.*17.5), limits = c(0, 35)) +
    scale_colour_manual(values=c('dodgerblue4', "firebrick1")) +
    scale_fill_manual(values=c('dodgerblue', "firebrick1")) +
    theme_bw() +
    labs(title = "Msambweni") +
    ylab("") +
    xlab("") +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_blank(), 
          axis.text = element_text(size = 10),
          axis.title.x = element_text(size = 14),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.y = element_text(size = 14), 
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
          legend.position = "none") + 
    scale_x_discrete(breaks = site_msa$MnYr, labels = site_msa$MonLa) +
    geom_vline(xintercept = 12.5, linetype = 2, color = "black", size = 0.25) +
    geom_vline(xintercept = 24.5, linetype = 2, color = "black", size = 0.25) +
    geom_vline(xintercept = 36.5, linetype = 2, color = "black", size = 0.25)
  
  temp_chu <- ggplot(site_chu, aes(x = MnYr)) + 
    geom_bar(aes(group = 1, y = (Rain/17.5), colour = 'Rainfall'), fill = 'dodgerblue', stat = 'identity') +
    geom_line(aes(group = 1, y = Temp, colour = 'Temperature')) +
    scale_y_continuous(sec.axis = sec_axis(~.*17.5), limits = c(0, 35)) +
    scale_colour_manual(values=c('dodgerblue4', "firebrick1")) +
    scale_fill_manual(values=c('dodgerblue', "firebrick1")) +
    theme_bw() +
    labs(title = "Chulaimbo") + 
    ylab("") +
    xlab("") +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_blank(), 
          axis.text = element_text(size = 10),
          axis.title.x = element_text(size = 14),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.y = element_text(size = 14), 
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
          legend.position = "none") + 
    scale_x_discrete(breaks = site_chu$MnYr, labels = site_chu$MonLa) +
    geom_vline(xintercept = 12.5, linetype = 2, color = "black", size = 0.25) +
    geom_vline(xintercept = 24.5, linetype = 2, color = "black", size = 0.25) +
    geom_vline(xintercept = 36.5, linetype = 2, color = "black", size = 0.25)
  
  full <- ggarrange(temp_kis, temp_chu, temp_uku, temp_msa, ncol = 1, nrow = 4, common.legend = TRUE, legend = "bottom")

  setEPS()
  postscript("Plots/Temprain.eps",  width = 10, height = 13)
  plot(full)
  annotate_figure(full,
                  top = text_grob("Variation in Temperature and Rainfall by Site", size = 28),
                  bottom = text_grob("Month", size = 24, vjust = 0.5),
                  left = text_grob(expression(paste('Temperature (',~degree,'C)',sep='')), rot = 90, size = 20, vjust = 1),
                  right = text_grob("Rainfall (mm)", size = 20, rot = 270, vjust = 0.5)
  )
  dev.off()
  #setEPS()
  # postscript("Plots/Temperatures.eps",  width = 10, height = 13)
  # grid.arrange(temp_kis, temp_chu, temp_msa, temp_uku,
  #              nrow = 4,
  #              top = grid::textGrob("Variation in Temperature and Rainfall by Site", gp = grid::gpar(fontsize = 22)), 
  #              left = grid::textGrob(expression(paste('Temperature (',~degree,'C)',sep='')), gp = grid::gpar(fontsize = 18), rot = 90), 
  #              right = grid::textGrob("Rainfall (mm)", gp = grid::gpar(fontsize = 18), rot = 270),
  #              bottom = grid::textGrob("Month", gp = grid::gpar(fontsize = 18)))
  # dev.off()
}

#Rain
{
  rain_kis <- ggplot(site_kis, aes(x = MnYr, y = Rain)) + 
    geom_line(aes(group = 1)) +
    theme_bw() +
    labs(title = "Kisumu") +
    ylab("") +
    xlab("") +
    ylim(c(0, 500)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_blank(), 
          axis.text = element_text(size = 10),
          axis.title.x = element_text(size = 14),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.y = element_text(size = 14), 
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
          legend.position = "none") + 
    scale_x_discrete(breaks = site_kis$MnYr, labels = site_kis$MonLa) +
    geom_vline(xintercept = 12.5, linetype = 2, color = "black", size = 0.25) +
    geom_vline(xintercept = 24.5, linetype = 2, color = "black", size = 0.25) +
    geom_vline(xintercept = 36.5, linetype = 2, color = "black", size = 0.25) +
    annotate(geom = "text", x= 6, y = 290, label = "2014 - 2015", color = "black", fontface = "bold", size = 5) +
    annotate(geom = "text", x= 18, y = 290, label = "2015 - 2016", color = "black", fontface = "bold", size = 5) +
    annotate(geom = "text", x= 30, y = 290, label = "2016 - 2017", color = "black", fontface = "bold", size = 5) +
    annotate(geom = "text", x= 42, y = 290, label = "2017 - 2018", color = "black", fontface = "bold", size = 5)
  
  rain_uku <- ggplot(site_uku, aes(x = MnYr, y = Rain)) + 
    geom_line(aes(group = 1)) +
    theme_bw() +
    labs(title = "Ukunda") +
    xlab("") +
    ylab("") +
    ylim(c(0, 500)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_blank(), 
          axis.text = element_text(size = 10),
          axis.title.x = element_text(size = 14),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.y = element_text(size = 14), 
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
          legend.position = "none") + 
    scale_x_discrete(breaks = site_uku$MnYr, labels = site_uku$MonLa) +
    geom_vline(xintercept = 12.5, linetype = 2, color = "black", size = 0.25) +
    geom_vline(xintercept = 24.5, linetype = 2, color = "black", size = 0.25) +
    geom_vline(xintercept = 36.5, linetype = 2, color = "black", size = 0.25)
  
  rain_msa <- ggplot(site_msa, aes(x = MnYr, y = Rain)) +
    geom_line(aes(group = 1)) +
    theme_bw() +
    labs(title = "Msambweni") +
    ylab("") +
    xlab("") +
    ylim(c(0, 500)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_blank(), 
          axis.text = element_text(size = 10),
          axis.title.x = element_text(size = 14),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.y = element_text(size = 14), 
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
          legend.position = "none") + 
    scale_x_discrete(breaks = site_msa$MnYr, labels = site_msa$MonLa) +
    geom_vline(xintercept = 12.5, linetype = 2, color = "black", size = 0.25) +
    geom_vline(xintercept = 24.5, linetype = 2, color = "black", size = 0.25) +
    geom_vline(xintercept = 36.5, linetype = 2, color = "black", size = 0.25)
  
  rain_chu <- ggplot(site_chu, aes(x = MnYr, y = Rain)) + 
    geom_line(aes(group = 1)) +
    theme_bw() +
    labs(title = "Chulaimbo") + 
    ylab("") +
    xlab("") +
    ylim(c(0, 500)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_blank(), 
          axis.text = element_text(size = 10),
          axis.title.x = element_text(size = 14),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.y = element_text(size = 14), 
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
          legend.position = "none") + 
    scale_x_discrete(breaks = site_chu$MnYr, labels = site_chu$MonLa) +
    geom_vline(xintercept = 12.5, linetype = 2, color = "black", size = 0.25) +
    geom_vline(xintercept = 24.5, linetype = 2, color = "black", size = 0.25) +
    geom_vline(xintercept = 36.5, linetype = 2, color = "black", size = 0.25)
  
  setEPS()
  postscript("Plots/Rainfall.eps",  width = 10, height = 13)
  grid.arrange(rain_kis, rain_chu, rain_msa, rain_uku,
               nrow = 4,
               top = grid::textGrob("Site Rainfall", gp = grid::gpar(fontsize = 18)))
  dev.off()
}

#****************************************************************************************#
#****************************************************************************************#
#Temp Plots
#Function. 
effect_plots_temp <- function(data, this_label) {
  
  effect_plot <- ggplot(data, aes(x = data[,1], y = exp(data[,2]))) +
    geom_line() + 
    geom_ribbon(aes(ymin = exp(data[,3]), ymax = exp(data[,7])), alpha=0.2) + 
    labs(title = 'Temperature') +
    xlab(expression(paste('Temperature (',~degree,'C)',sep=''))) +
    ylab("Odds Ratio (95% CI)") +
    scale_y_continuous(limits = c(0, 4.0), breaks = c(0, 1, 2, 3, 4)) +
    scale_x_continuous(limits = c(22, 32), breaks = c(22, 24, 26, 28, 30, 32)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          legend.title = element_blank(), 
          axis.text = element_text(size = 16),
          axis.title.x = element_text(size = 18),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.y = element_text(size = 18), 
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
          legend.position = "none") +
    geom_hline(yintercept = 1, linetype = 2, color = "black", size = 0.5)
  
  return(effect_plot)
  
}
effect_plots_rain <- function(data, this_label) {
  
  effect_plot <- ggplot(data, aes(x = data[,1], y = exp(data[,2]))) +
    geom_line() + 
    geom_ribbon(aes(ymin = exp(data[,3]), ymax = exp(data[,7])), alpha=0.2) + 
    labs(title = 'Rainfall') +
    xlab("Rainfall (mm)") +
    ylab("") +
    scale_y_continuous(limits = c(0, 4.0), breaks = c(0, 1, 2, 3, 4)) +
    scale_x_continuous(limits = c(0, 500), breaks = c(0, 100, 200, 300, 400, 500)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          legend.title = element_blank(), 
          axis.text = element_text(size = 16),
          axis.title.x = element_text(size = 18),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.y = element_text(size = 18), 
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
          legend.position = "none") +
    geom_hline(yintercept = 1, linetype = 2, color = "black", size = 0.5)
  
  return(effect_plot)
  
}

#Abundance full. Temp.
data_abu_full_temp <- ab_full_trm$effects$`sx(Temp)`
plot_abu_full_temp <- effect_plots_temp(data_abu_full_temp, "Overall")
plot(plot_abu_full_temp)

#Abundance full. Rain.
data_abu_full_rain <- ab_full_trm$effects$`sx(Rain)`
plot_abu_full_rain <- effect_plots_rain(data_abu_full_rain, "Overall")
plot(plot_abu_full_rain)

#Arrange.
jpeg("Plots/Temprain Effect.jpg",  width = 1000, height = 600)
grid.arrange(plot_abu_full_temp, plot_abu_full_rain, nrow = 1, ncol = 2, 
             top = grid::textGrob("Effect of Temperature and Rainfall on abundance", gp = grid::gpar(fontsize = 28)))
dev.off()
#****************************************************************************************#
#****************************************************************************************#
#Sensitivity Annalysis forest plots. 







