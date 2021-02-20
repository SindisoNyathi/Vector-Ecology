#********************************************************************************#
#20/12/2019
#Sindiso Nyathi
#Goal: Format VEctor Results for reporting.
#********************************************************************************#

#********************************************************************************#
#Load packages, set directories and read in required files.
#Set working directory, read in files and set libraries.
setwd("~/Box/Sindiso Nyathi's Files/Vector Control/Whole Data")

#Load the required libraries
these_packages <- c("ggplot2", "ggthemes", "tidyverse", "naniar", "magrittr", "lubridate", 
                    "ggpubr", "PNWColors", "gridExtra", 'xlsx')
lapply(these_packages, require, character.only = TRUE)

#Full. 
ab_full_trm <- readRDS("Model Results/Abundance Model. Full. Mult. TRM.rds")
ab_wes_trm <- readRDS("Model Results/Abundance Model. West. Mult. TRM.rds")
ab_coa_trm <- readRDS("Model Results/Abundance Model. Coast. Mult. TRM.rds")
pers_full <- readRDS("Model Results/Persistence Model. Full.rds")

#Sensitivity Analyses Data. 
replacements_sa <- readRDS("Model Results/Abundance Model. Full. Mult. SA.rds")
outliers_sa <- readRDS("Model Results/Abundance Model. Mult. SA. Outliers.rds")
#********************************************************************************#

#********************************************************************************#
#File to ormat and save the model files.
formres <- function(curr_model){
  
  #Odds ratios first. 
  ab_results <- as.data.frame(matrix(nrow = 17, ncol = 6))
  
  colnames(ab_results) <- c("Coeff", "Est.", "Lower", "Upper", "P-value", "StdErr")
  
  #Housewall
  housewall <- curr_model$effects[which(names(curr_model$effects) == 'Housewall')][[1]][[1]]
  ab_results[1,] <- c('Housewall', 
                      housewall[2], 
                      housewall[3], 
                      housewall[7],
                      housewall[10], 
                      housewall[5])
  
  #Houseroof 2
  houseroof <- curr_model$effects[which(names(curr_model$effects) == 'Houseroof')][[1]][[1]]
  ab_results[2,] <- c('Houseroof', 
                      houseroof[2], 
                      houseroof[3], 
                      houseroof[7],
                      houseroof[10], 
                      houseroof[5])
  
  #Houseroof 3
  houseroof2 <- curr_model$effects[which(names(curr_model$effects) == 'Houseroof')][[1]][[2]]
  ab_results[3,] <- c('Houseroof2', 
                      houseroof2[2], 
                      houseroof2[3], 
                      houseroof2[7],
                      houseroof2[10], 
                      houseroof2[5])
  
  
  #Firewood
  firewood <- curr_model$effects[which(names(curr_model$effects) == 'Firewood')][[1]][[1]]
  ab_results[4,] <- c('Firewood', 
                      firewood[2], 
                      firewood[3], 
                      firewood[7],
                      firewood[10], 
                      firewood[5])
  
  #Insmos
  if ('Insmos' %in% names(curr_model$effects)){
    insmos <- curr_model$effects[which(names(curr_model$effects) == 'Insmos')][[1]][[1]]
    ab_results[5,] <- c('Insmos', 
                      insmos[2], 
                      insmos[3], 
                      insmos[7],
                      insmos[10], 
                      insmos[5])
  }
  
  #Rooms3 - 4
  rooms <- curr_model$effects[which(names(curr_model$effects) == 'RoomsB')][[1]][[1]]
  ab_results[6,] <- c('Rooms', 
                      rooms[2], 
                      rooms[3], 
                      rooms[7],
                      rooms[10], 
                      rooms[5])
  
  
  #Rooms4 or more
  rooms2 <- curr_model$effects[which(names(curr_model$effects) == 'RoomsB')][[1]][[2]]
  ab_results[7,] <- c('Rooms2', 
                      rooms2[2], 
                      rooms2[3], 
                      rooms2[7],
                      rooms2[10], 
                      rooms2[5])
  
  #Eaves
  eaves <- curr_model$effects[which(names(curr_model$effects) == 'Eaves')][[1]][[1]]
  ab_results[8,] <- c('Eaves', 
                      eaves[2], 
                      eaves[3], 
                      eaves[7],
                      eaves[10], 
                      eaves[5])

  
  #Ceilings
  ceilings <- curr_model$effects[which(names(curr_model$effects) == 'Ceilings')][[1]][[1]]
  ab_results[9,] <- c('Ceilings', 
                      ceilings[2], 
                      ceilings[3], 
                      ceilings[7],
                      ceilings[10], 
                      ceilings[5])
  
  #Sleepers 4 - 6
  sleepers <- curr_model$effects[which(names(curr_model$effects) == 'SleepersB')][[1]][[1]]
  ab_results[10,] <- c('Sleepers', 
                      sleepers[2], 
                      sleepers[3], 
                      sleepers[7],
                      sleepers[10], 
                      sleepers[5])
  
  
  #Sleepers 6 or more
  sleepers2 <- curr_model$effects[which(names(curr_model$effects) == 'SleepersB')][[1]][[2]]
  ab_results[11,] <- c('Sleepers2', 
                       sleepers2[2], 
                       sleepers2[3], 
                       sleepers2[7],
                       sleepers2[10], 
                       sleepers2[5])
  
  #Growth
  growth <- curr_model$effects[which(names(curr_model$effects) == 'Growth')][[1]][[1]]
  ab_results[12,] <- c('Growth', 
                       growth[2], 
                       growth[3], 
                       growth[7],
                       growth[10], 
                       growth[5])

  #Urban
  urban <- curr_model$effects[which(names(curr_model$effects) == 'Urban')][[1]][[1]]
  ab_results[13,] <- c('Urban', 
                       urban[2], 
                       urban[3], 
                       urban[7],
                       urban[10], 
                       urban[5])
  
  #Habitat Count. 
  habitatcount <- curr_model$effects[which(names(curr_model$effects) == 'HC')][[1]]
  ab_results[14,] <- c('HC', 
                       habitatcount[1, 2], 
                       habitatcount[1, 3], 
                       habitatcount[1, 7],
                       habitatcount[1, 10], 
                       habitatcount[1, 5])
  
  # #Lag 1.
  # lag1 <- curr_model$effects[which(names(curr_model$effects) == 'Lag1')][[1]]
  # ab_results[15,] <- c('Lag1', 
  #                      lag1[1, 2], 
  #                      lag1[1, 3], 
  #                      lag1[1, 7],
  #                      lag1[1, 10], 
  #                      lag1[1, 5])
  # 
  # #Lag 2
  # lag2 <- curr_model$effects[which(names(curr_model$effects) == 'Lag2')][[1]]
  # ab_results[16,] <- c('Lag2', 
  #                      lag2[1, 2], 
  #                      lag2[1, 3], 
  #                      lag2[1, 7],
  #                      lag2[1, 10], 
  #                      lag2[1, 5])
  # 
  #Location
  if ('Location' %in% names(curr_model$effects)){
    location <- curr_model$effects[which(names(curr_model$effects) == 'Location')][[1]][[1]]
    ab_results[17,] <- c('Location', 
                       location[1, 2], 
                       location[1, 3], 
                       location[1, 7],
                       location[1, 10], 
                       location[1, 5])
  }
  
  #Don't remove NAs
  #if (any(is.na(ab_results$Coeff)) == 'TRUE'){
  #  ab_results <- ab_results[-which(is.na(ab_results$Coeff)),]
  #}
  
  ab_results[, c(2:6)] <- lapply(ab_results[, c(2:6)] , as.character)
  ab_results[, c(2:6)] <- lapply(ab_results[, c(2:6)] , as.numeric)
  ab_results[, c(2, 3, 4, 6)] <- exp(ab_results[, c(2, 3, 4, 6)])
  
  return(ab_results)
  
}
#********************************************************************************#

#********************************************************************************#
full <- formres(ab_full_trm)
wes <- formres(ab_wes_trm)
coa <- formres(ab_coa_trm)
pers <- formres(pers_full)

rep_sa <- formres(replacements_sa)
out_sa <- formres(outliers_sa)

write.csv(full, 'Model Results/Abundance. Full. Coefficients.csv')
write.csv(wes, 'Model Results/Abundance. West. Coefficients.csv')
write.csv(coa, 'Model Results/Abundance. Coast. Coefficients.csv')
write.csv(pers, 'Model Results/Persistence. Coefficients.csv')

write.csv(rep_sa, 'Model Results/Replacement SA Coefficients.csv')
write.csv(out_sa, 'Model Results/Outliers SA. Coefficients.csv')
#********************************************************************************#

#********************************************************************************#