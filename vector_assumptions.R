#********************************************************************************#
#11/02/2020
#Sindiso Nyathi
#Goal: Checking the proportional Odds assumptions and partial residuals. 
#********************************************************************************#

#********************************************************************************#
#Preliminaries.
#Set the working directory.
setwd("~/Box/Sindiso Nyathi's Files/Vector Control/Whole Data")

#Load the required libraries. This may require you to install some of these. 
these_packages <- c("naniar", "magrittr", "lubridate", "ggpubr", 
                    "writexl", "reshape2", "synchrony",  "R2BayesX", 
                    "itsadug", "MASS", "rgdal", "raster", "sf", "BayesX")


#Load the required packages.
lapply(these_packages, require, character.only = TRUE)

#Read in the datafile.
abun <- readRDS("Data Files/Vector Abundance Dataset.rds")
abun_resid <- abun

#Add the indicator/dummy columns for the output. 
abun_resid$Zero <- NA
abun_resid$Low <- NA
abun_resid$Med <- NA

#The Zero Category. 
abun_resid$Zero[abun$PupaeMult == 0] <- 0
abun_resid$Zero[abun$PupaeMult == 1|abun$PupaeMult == 2| abun$PupaeMult == 3] <- 1
abun_resid$Zero[is.na(abun$PupaeMult)] <- NA

#The Low Category. 
abun_resid$Low[abun$PupaeMult == 0] <- 0
abun_resid$Low[abun$PupaeMult == 1] <- 0
abun_resid$Low[abun$PupaeMult == 2| abun$PupaeMult == 3] <- 1
abun_resid$Low[is.na(abun$PupaeMult)] <- NA

#The Intermediates Category. 
abun_resid$Med[abun$PupaeMult == 0] <- 0
abun_resid$Med[abun$PupaeMult == 1] <- 0
abun_resid$Med[abun$PupaeMult == 2] <- 0
abun_resid$Med[abun$PupaeMult == 3] <- 1
abun_resid$Med[is.na(abun$PupaeMult)] <- NA

pers <- readRDS("Data Files/Vector Persistence Dataset.rds")

#Split the site for coast and west models
coa <- abun[which(abun$Location == "Coast"),]
wes <- abun[which(abun$Location == "West"),]

#Read in the model. 
ab_full <- readRDS("Model Results/Abundance Model. Full. Mult.rds")
ab_wes <- readRDS("Model Results/Abundance Model. West. Full. Mult.rds")
ab_coa <- readRDS("Model Results/Abundance Model. Coast. Full. Mult.rds")
#********************************************************************************#

#********************************************************************************#
#Check Partial residuals. 
#We will compute these 1 by 1 for each predictor so as to avoid 
#any confusion or mistakes.
#Get the predicted values. 
ab_full_pred <- predict(ab_full)
ab_full_resid <- fitted(ab_full)
colnames(ab_full_pred) <- c('Zero', 'Low', 'Med')
pzero_hat <- ab_full_pred$Zero
plow_hat <- ab_full_pred$Low
pmed_hat <- ab_full_pred$Med

pzero_res <- ab_full_resid[,1]
plow_res <- ab_full_resid[,2]
pmed_res <- ab_full_resid[,3]
#********************************************************************************#

#********************************************************************************#
#Housewall. 
#Recode abundance Housewall. 
abun_resid$Housewall <- as.numeric(abun_resid$Housewall)
abun_resid$Housewall[abun_resid$Housewall == 1] <- 0
abun_resid$Housewall[abun_resid$Housewall == 2] <- 1
beta_housewall2 <- ab_full$effects$Housewall[[1]][[2]]

partial_residual_housewall_zero <- (beta_housewall2*abun_resid$Housewall) - ((abun_resid$Zero - pzero_hat)/(pzero_hat*(1-pzero_hat)))
partial_residual_housewall_low <- (beta_housewall2*abun_resid$Housewall) - ((abun_resid$Low - plow_hat)/(pzero_hat*(1-plow_hat)))
partial_residual_housewall_med <- (beta_housewall2*abun_resid$Housewall) - ((abun_resid$Med - pmed_hat)/(pzero_hat*(1-pmed_hat)))

plot(partial_residual_housewall_zero)
plot(partial_residual_housewall_low)
plot(partial_residual_housewall_med)

prop_odd_wall <- as.data.frame(cbind(abun_resid$Housewall, partial_residual_housewall_zero, 
                                     partial_residual_housewall_low, partial_residual_housewall_med))

ggplot(prop_odd_wall, aes(x = V1, y = partial_residual_housewall_zero)) +
  stat_summary(fun.y = mean, colour = "red", geom = "line", size = 0.25) + 
  stat_summary(aes(y = partial_residual_housewall_low), fun.y = mean, colour = "blue", geom = "line", size = 0.25) + 
  stat_summary(aes(y = partial_residual_housewall_med), fun.y = mean, colour = "black", geom = "line", size = 0.25)

probs_zero <- exp(partial_residual_housewall_zero)/(1+exp(partial_residual_housewall_zero))
probs_low <- exp(partial_residual_housewall_low)/(1+exp(partial_residual_housewall_low))
probs_med <- exp(partial_residual_housewall_med)/(1+exp(partial_residual_housewall_med))

prop_odd_wall_probs <- as.data.frame(cbind(abun_resid$Housewall, probs_zero, 
                                           probs_low, probs_med))

ggplot(prop_odd_wall_probs, aes(x = V1, y = probs_zero)) +
  #geom_point() +
  stat_summary(fun.y = mean, colour = "red", geom = "line", size = 0.25) + 
  stat_summary(aes(y = probs_low), fun.y = mean, colour = "blue", geom = "line", size = 0.25) + 
  stat_summary(aes(y = probs_med), fun.y = mean, colour = "black", geom = "line", size = 0.25)

#Partial Residuals. Not very useful for a bonary outcome. 
ggplot(prop_odd_wall, aes(x = V1, y = partial_residual_housewall_zero)) + geom_point()
ggplot(prop_odd_wall, aes(x = V1, y = partial_residual_housewall_low)) + geom_point()
ggplot(prop_odd_wall, aes(x = V1, y = partial_residual_housewall_med)) + geom_point()
#********************************************************************************#

#********************************************************************************#
#Habitat Count. 
beta_habitat <- ab_full$effects$HC[[2]][[1]]

partial_residual_habitat_zero <- (beta_habitat*abun_resid$HC) - ((abun_resid$Zero - pzero_hat)/(pzero_hat*(1-pzero_hat)))
partial_residual_habitat_low <- (beta_habitat*abun_resid$HC) - ((abun_resid$Low - pzero_hat)/(pzero_hat*(1-plow_hat)))
partial_residual_habitat_med <- (beta_habitat*abun_resid$HC) - ((abun_resid$Med - pzero_hat)/(pzero_hat*(1-pmed_hat)))

plot(partial_residual_habitat_zero)
plot(partial_residual_habitat_low)
plot(partial_residual_habitat_med)

prop_odd_hc <- as.data.frame(cbind(abun_resid$HC, partial_residual_habitat_zero, 
                                   partial_residual_habitat_low, partial_residual_habitat_med))

ggplot(prop_odd_hc, aes(x = V1, y = partial_residual_habitat_zero)) +
  stat_summary(fun.y = mean, colour = "red", geom = "line", size = 0.25) + 
  stat_summary(aes(y = partial_residual_habitat_low), fun.y = mean, colour = "blue", geom = "line", size = 0.25) + 
  stat_summary(aes(y = partial_residual_habitat_med), fun.y = mean, colour = "black", geom = "line", size = 0.25)

probs_zero <- exp(partial_residual_habitat_zero)/(1+exp(partial_residual_habitat_zero))
probs_low <- exp(partial_residual_habitat_low)/(1+exp(partial_residual_habitat_low))
probs_med <- exp(partial_residual_habitat_med)/(1+exp(partial_residual_habitat_med))

prop_odd_hc_probs <- as.data.frame(cbind(abun_resid$HC, probs_zero, 
                                           probs_low, probs_med))

ggplot(prop_odd_hc_probs, aes(x = V1, y = probs_zero)) +
  #geom_point() +
  stat_summary(fun.y = mean, colour = "red", geom = "line", size = 0.25) + 
  stat_summary(aes(y = probs_low), fun.y = mean, colour = "blue", geom = "line", size = 0.25) + 
  stat_summary(aes(y = probs_med), fun.y = mean, colour = "black", geom = "line", size = 0.25)

#Partial Residuals.
ggplot(prop_odd_hc, aes(x = V1, y = partial_residual_habitat_zero)) + geom_point() 
ggplot(prop_odd_hc, aes(x = V1, y = partial_residual_habitat_low)) + geom_point()
ggplot(prop_odd_hc, aes(x = V1, y = partial_residual_habitat_med)) + geom_point()
#These plots seem a little odd to me. 
#In general the plots seem to make sense but these last ones. 
#********************************************************************************#

#********************************************************************************#
#RoomsA 

#********************************************************************************#

#********************************************************************************#

#********************************************************************************#

#********************************************************************************#

#********************************************************************************#

#********************************************************************************#
#Residuals for the persistence model. 
pers_model <- readRDS("Model Results/Persistence Model. Full.rds")
pers_residuals <- as.data.frame(pers_model$residuals)
pers_pred <- predict(pers_model)
pers_pred <- pers_pred[!is.na(pers_pred)]
pers_residuals$pers_pred <- pers_pred

hist(pers_residuals[, 1], breaks = 20)
plot(pers_residuals[, 1])
