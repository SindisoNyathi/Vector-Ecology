#**************************************************************************************#
#15/05
#Sindiso Nyathi.
#Goal: GAMM Abundance and Persistence Models with R2BayesX
#Final May 15th Check.
#********************************************************************************#

#********************************************************************************#
# #Preliminaries.
#Set the working directory.
setwd()

#Load the required libraries. This may require you to install some of these. 
these_packages <- c("naniar", "magrittr", "lubridate", "ggpubr", 
                    "writexl", "reshape2", "synchrony",  "R2BayesX", 
                    "itsadug", "MASS", "rgdal", "raster", "sf", "BayesX")


#Load the required packages.
lapply(these_packages, require, character.only = TRUE)

#Read in the datafile.
abun <- readRDS("Data Files/Vector Abundance Dataset.rds")
abun$HouseID <- as.factor(abun$HouseID)
abun$Season <- as.factor(abun$Season)
pers <- readRDS("Data Files/Vector Persistence Dataset.rds")
pers$HouseID <- as.factor(pers$HouseID)

#Split the site for coast and west models
coa <- abun[which(abun$Location == "Coast"),]
wes <- abun[which(abun$Location == "West"),]

#SpatialFile. 
#Buffers
all_shp <- "Data Files/Spatial/Buffers"
unloadNamespace("shapefiles")
all_bnd <- shp2bnd(all_shp, regionnames = "HouseID")
#***************************************************************************************#

#****************************************************************************************#
#Model building with R2Bayesx.
#Outcome: Pupae Count
#Distribution: ZINB
#Model Set 1: Abundance Full models.

#Full model. Mult. TR. Month. ###Final Model###
{
  abundance_full_mult_trm <- bayesx(PupaeMult ~ 
                                      
                                      #Random Effects
                                      sx(HouseID, bs = 'ra', knots = 80) +
                                      sx(Site, bs = 'ra', knots = 4) +
                                      
                                      #Spatial Term
                                      sx(HouseID, bs = 'gs', map = all_bnd) +
                                      
                                      #Non-linear effects
                                      sx(Temp, bs = 'ps', knots = 5) +
                                      sx(Rain, bs = 'ps', knots = 5) +
                                      sx(MonthNum, by = YearNum, bs = 'season', period = 12) +
                                      sx(YearNum, bs = 'ps') +
                                      
                                      #Linear Effects
                                      Housewall +
                                      Houseroof +
                                      Firewood +
                                      Insmos +
                                      RoomsB + 
                                      Eaves +
                                      Ceilings +
                                      SleepersB + 
                                      Growth +
                                      HC + 
                                      Location +
                                      Urban,
                                    
                                    #Model
                                    family = "cumlogit",
                                    method = "REML",

                                    #Data
                                    data = abun
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_full_mult_trm, "Model Results/Abundance Model. Full. Mult. TRM.rds")
  bayesx_logfile(abundance_full_mult_trm)
  summary(abundance_full_mult_trm)
}

#Full model. Mult. Base.  
{
  abundance_full_mult <- bayesx(PupaeMult ~ 
                             
                             #Random Effects
                             sx(HouseID, bs = 'ra', knots = 80) +
                             sx(Site, bs = 'ra', knots = 4) +
                             
                             #Spatial Term
                             sx(HouseID, bs = 'gs', map = all_bnd) +
                             
                             #Non-linear effects
                             sx(MonthNum, by = YearNum, bs = 'season', period = 12) +
                             sx(YearNum, bs = 'ps') +
                               
                             #Linear Effects
                             Housewall +
                             Houseroof +
                             Firewood +
                             Insmos +
                             RoomsB + 
                             Eaves +
                             Ceilings +
                             SleepersB + 
                             Growth +
                             HC + 
                             Location +
                             Urban,
                           
                           #Model
                           family = "cumlogit",
                           method = "REML",
                           
                           #Data
                           data = abun
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_full_mult, "Model Results/Abundance Model. Full. Mult.rds")
  bayesx_logfile(abundance_full_mult)
  summary(abundance_full_mult)
}

#Full model. Mult. TR.   
{
  abundance_full_mult_tr <- bayesx(PupaeMult ~ 
                                  
                                  #Random Effects
                                  sx(HouseID, bs = 'ra', knots = 80) +
                                  sx(Site, bs = 'ra', knots = 4) +
                                  
                                  #Spatial Term
                                  sx(HouseID, bs = 'gs', map = all_bnd) +
                                  
                                  #Non-linear effects
                                  sx(Temp, bs = 'ps', knots = 5) +
                                  sx(Rain, bs = 'ps', knots = 5) +
                                  sx(YearNum, bs = 'ps') + 
                                  
                                  #Linear Effects
                                  Housewall +
                                  Houseroof +
                                  Firewood +
                                  Insmos +
                                  RoomsB + 
                                  Eaves +
                                  Ceilings +
                                  SleepersB + 
                                  Growth +
                                  HC + 
                                  Location +
                                  Urban,
                                
                                #Model
                                family = "cumlogit",
                                method = "REML",

                                #Data
                                data = abun
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_full_mult_tr, "Model Results/Abundance Model. Full. Mult. TR.rds")
  bayesx_logfile(abundance_full_mult_tr)
  summary(abundance_full_mult_tr)
}

#Full model. Mult. Lags   
{
  abundance_full_mult_lag <- bayesx(PupaeMult ~ 
                                  
                                  #Random Effects
                                  sx(HouseID, bs = 'ra', knots = 80) +
                                  sx(Site, bs = 'ra', knots = 4) +
                                  
                                  #Spatial Term
                                  sx(HouseID, bs = 'gs', map = all_bnd) +
                                  
                                  #Non-linear effects
                                  sx(MonthNum, by = YearNum, bs = 'season', period = 12) +
                                  sx(YearNum, bs = 'ps', knots = 5) +  
                                  
                                  #Linear Effects
                                  Housewall +
                                  Houseroof +
                                  Firewood +
                                  Insmos +
                                  RoomsB + 
                                  Eaves +
                                  Ceilings +
                                  SleepersB + 
                                  Growth +
                                  HC + 
                                  Location +
                                  Urban + 
                                  Lag1 + Lag2,
                                
                                #Model
                                family = "cumlogit",
                                method = "REML",

                                #Data
                                data = abun
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_full_mult_lag, "Model Results/Abundance Model. Full. Mult. Lag.rds")
  bayesx_logfile(abundance_full_mult_lag)
  summary(abundance_full_mult_lag)
}

#Full model. Mult. TR. Month. Lag.
{
  abundance_full_bin_trm_lag <- bayesx(PupaeMult ~ 
                                     
                                     #Random Effects
                                     sx(HouseID, bs = 'ra', knots = 80) +
                                     sx(Site, bs = 'ra', knots = 4) +
                                     
                                     #Spatial Term
                                     sx(HouseID, bs = 'gs', map = all_bnd) +
                                     
                                     #Non-linear effects
                                     sx(Temp, bs = 'ps', knots = 5) +
                                     sx(Rain, bs = 'ps', knots = 5) + 
                                     sx(MonthNum, by = YearNum, bs = 'season', period = 12) +
                                     sx(YearNum, bs = 'ps', knots = 5) +
                                       
                                     #Linear Effects
                                     Housewall +
                                     Houseroof +
                                     Firewood +
                                     Insmos +
                                     RoomsB + 
                                     Eaves +
                                     Ceilings +
                                     SleepersB + 
                                     Growth +
                                     HC + 
                                     Location +
                                     Lag1 + Lag2 +
                                     Urban,
                                   
                                   #Model
                                   family = "cumlogit",
                                   method = "REML",

                                   #Data
                                   data = abun
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_full_bin_trm_lag, "Model Results/Abundance Model. Full. Bin. TRM. Lag.rds")
  bayesx_logfile(abundance_full_bin_trm_lag)
  summary(abundance_full_bin_trm_lag)
}

#Full model. Mult. Space.
{
  abundance_full_mult_ns <- bayesx(PupaeMult ~ 
                                         
                                         #Random Effects
                                         sx(HouseID, bs = 'ra', knots = 80) +
                                         sx(Site, bs = 'ra', knots = 4) +
                                         
                                         #Non-linear effects
                                         sx(MonthNum, by = YearNum, bs = 'season', period = 12) + 
                                         sx(YearNum, bs = 'ps', knots = 5) +
                                         
                                         #Linear Effects
                                         Housewall +
                                         Houseroof +
                                         Firewood +
                                         Insmos +
                                         RoomsB + 
                                         Eaves +
                                         Ceilings +
                                         SleepersB + 
                                         Growth +
                                         HC + 
                                         Location +
                                         Urban,
                                       
                                       #Model
                                       family = "cumlogit",
                                       method = "REML",
                                       
                                       #Data
                                       data = abun
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_full_mult_ns, "Model Results/Abundance Model. Full. Mult. NS.rds")
  bayesx_logfile(abundance_full_mult_ns)
  summary(abundance_full_mult_ns)
}

#Residuals Binomial Models. 
#Full model. Low
{
  abundance_full_bin_res_low <- bayesx(Low ~ 
                                        
                                         #Random Effects
                                         sx(HouseID, bs = 'ra', knots = 80) +
                                         sx(Site, bs = 'ra', knots = 4) +
                                         
                                         #Spatial Term
                                         sx(HouseID, bs = 'gs', map = all_bnd) +
                                         
                                         #Non-linear effects
                                         sx(Temp, bs = 'ps', knots = 5) +
                                         sx(Rain, bs = 'ps', knots = 5) +
                                         sx(MonthNum, by = YearNum, bs = 'season', period = 12) +
                                         sx(YearNum, bs = 'ps', knots = 5) +
                                         
                                         #Linear Effects
                                         Housewall +
                                         Houseroof +
                                         Firewood +
                                         Insmos +
                                         RoomsB + 
                                         Eaves +
                                         Ceilings +
                                         SleepersB + 
                                         Growth +
                                         HC + 
                                         Location +
                                         Urban,
                                      
                                      #Model
                                      family = "binomial",
                                      method = "REML",

                                      #Data
                                      data = abun
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_full_bin_res_low, "Model Results/Abundance Model. Full. Bin. Residuals. Low.rds")
  bayesx_logfile(abundance_full_bin_res_low)
  summary(abundance_full_bin_res_low)
}

#Full model. Int
{
  abundance_full_bin_res_int <- bayesx(Int ~ 
                                         
                                         #Random Effects
                                         sx(HouseID, bs = 'ra', knots = 80) +
                                         sx(Site, bs = 'ra', knots = 4) +
                                         
                                         #Spatial Term
                                         sx(HouseID, bs = 'gs', map = all_bnd) +
                                         
                                         #Non-linear effects
                                         sx(Temp, bs = 'ps', knots = 5) +
                                         sx(Rain, bs = 'ps', knots = 5) +
                                         sx(MonthNum, by = YearNum, bs = 'season', period = 12) +
                                         sx(YearNum, bs = 'ps', knots = 5) +
                                         
                                         #Linear Effects
                                         Housewall +
                                         Houseroof +
                                         Firewood +
                                         Insmos +
                                         RoomsB + 
                                         Eaves +
                                         Ceilings +
                                         SleepersB + 
                                         Growth +
                                         HC + 
                                         Location +
                                         Urban,
                                       
                                       #Model
                                       family = "binomial",
                                       method = "REML",
                                       
                                       #Data
                                       data = abun
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_full_bin_res_int, "Model Results/Abundance Model. Full. Bin. Residuals. Int.rds")
  bayesx_logfile(abundance_full_bin_res_int)
  summary(abundance_full_bin_res_int)
}

#Full model. High
{
  abundance_full_bin_res_hig <- bayesx(Hig ~ 
                                         
                                         #Random Effects
                                         sx(HouseID, bs = 'ra', knots = 80) +
                                         sx(Site, bs = 'ra', knots = 4) +
                                         
                                         #Spatial Term
                                         sx(HouseID, bs = 'gs', map = all_bnd) +
                                         
                                         #Non-linear effects
                                         sx(Temp, bs = 'ps', knots = 5) +
                                         sx(Rain, bs = 'ps', knots = 5) +
                                         sx(MonthNum, by = YearNum, bs = 'season', period = 12) +
                                         sx(YearNum, bs = 'ps', knots = 5) +
                                         
                                         #Linear Effects
                                         Housewall +
                                         Houseroof +
                                         Firewood +
                                         Insmos +
                                         RoomsB + 
                                         Eaves +
                                         Ceilings +
                                         SleepersB + 
                                         Growth +
                                         HC + 
                                         Location +
                                         Urban,
                                       
                                       #Model
                                       family = "binomial",
                                       method = "REML",
                                       
                                       #Data
                                       data = abun
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_full_bin_res_hig, "Model Results/Abundance Model. Full. Bin. Residuals. Hig.rds")
  bayesx_logfile(abundance_full_bin_res_hig)
  summary(abundance_full_bin_res_hig)
}

#Full model. Poisson.   
{
  abundance_full <- bayesx(Pupae ~ 
                             
                             #Random Effects
                             sx(HouseID, by = Site, bs = 'ra', knots = 80) +
                             
                             #Spatial Term
                             sx(HouseID, bs = 'gs', map = all_bnd) +
                             
                             #Non-linear effects
                             sx(Habitatcount, bs = "ps", knots = 5) + 
                             sx(Month, bs = 'ps', knots = 5) +
                             sx(YearName, bs = 'ps') +  
                             
                             #Linear Effects
                             Housewall +
                             Houseroof +
                             Firewood +
                             Insmos +
                             RoomsB + 
                             Eaves +
                             Ceilings +
                             SleepersB + 
                             Growth +
                             Location +
                             Urban,
                           
                           #Model
                           zipdistopt = "zinb",
                           family = "poisson",
                           method = "REML",
                           maxiter = 200,
                           
                           #Data
                           data = abun
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_full, "Model Results/Abundance Model. Full.rds")
  bayesx_logfile(abundance_full)
  summary(abundance_full)
}
#****************************************************************************************#

#****************************************************************************************#
#Model Set 2: Abundance West Models. 

#West model. Mult. Month. TR. ##Main Model###
{
  abundance_west_mult_trm <- bayesx(PupaeMult ~ 
                                          
                                      #Random Effects
                                      sx(HouseID, bs = 'ra', knots = 40) +
                                      sx(Site, bs = 'ra', knots = 2) +
                                      
                                      #Spatial Term
                                      sx(HouseID, bs = 'gs', map = all_bnd) +
                                      
                                      #Non-linear effects
                                      sx(Temp, bs = 'ps', knots = 5) +
                                      sx(Rain, bs = 'ps', knots = 5) +
                                      sx(MonthNum, by = YearNum, bs = 'season', period = 12) +
                                      sx(YearNum, bs = 'ps') +
                                      
                                      #Linear Effects
                                      Housewall +
                                      Houseroof +
                                      Firewood +
                                      #Insmos +
                                      RoomsB + 
                                      Eaves +
                                      Ceilings +
                                      SleepersB + 
                                      Growth +
                                      HC + 
                                      #Location +
                                      Urban,
                                        
                                      family  = "cumlogit",
                                      method = "REML",
                                    
                                      #Data.
                                      data = wes
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_west_mult_trm, "Model Results/Abundance Model. West. Mult. TRM.rds")
  bayesx_logfile(abundance_west_mult_trm)
  summary(abundance_west_mult_trm)
}

#West model. Mult. Base. 
{
  abundance_west_full_mult <- bayesx(PupaeMult ~ 
                                  
                                  #Random Effects
                                  sx(HouseID, by = Site, bs = 'ra', nrknots = 80) +
                                  
                                  #Space via geosplines 
                                  sx(HouseID, bs = "gs", map = all_bnd) +
                                  
                                  #Non-linear effects
                                  sx(Month, by = YearName,bs = "ps", nrknots = 5, degree = 3) +
                                  sx(YearName, bs = 'ps') +

                                  #Linear Effects
                                  Housewall +
                                  Houseroof +
                                  Firewood +
                                  #Insmos +
                                  #Bednet +
                                  SleepersB +
                                  RoomsB + 
                                  Eaves +
                                  HC +
                                  Ceilings +
                                  Growth,
                                  #Urban,
                                
                                family  = "cumlogit",
                                method = "REML",
                                #maxiter = 200,
                                
                                #Data.
                                data = wes
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_west_full_mult, "Model Results/Abundance Model. West. Full. Mult.rds")
  bayesx_logfile(abundance_west_full_mult)
  summary(abundance_west_full_mult)
}

#West model. Mult. TR. 
{
  abundance_west_full_mult_tr <- bayesx(PupaeMult ~ 
                                       
                                       #Random Effects
                                       sx(HouseID, by = Site, bs = 'ra', nrknots = 80) +
                                       
                                       #Space via geosplines 
                                       sx(HouseID, bs = "gs", map = all_bnd) +
                                       
                                       #Non-linear effects
                                       sx(Temp, Rain, bs = 'te', knots = 5) +
                                       #sx(Rain, by = YearName, bs = 'ps', knots = 5) + 
                                       sx(Month, bs = "ps", nrknots = 5, degree = 3) +
                                       sx(YearName, bs = 'ps') +
                                       
                                       #Linear Effects
                                       Housewall +
                                       Houseroof +
                                       Firewood +
                                       #Insmos +
                                       #Bednet +
                                       SleepersB +
                                       RoomsB + 
                                       Eaves +
                                       HC +
                                       Ceilings +
                                       Growth +
                                       Urban
                                       ,
                                     
                                     family  = "cumlogit",
                                     method = "REML",
                                     #maxiter = 200,
                                     
                                     #Data.
                                     data = wes
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_west_full_mult_tr, "Model Results/Abundance Model. West. Full. Mult. TR.rds")
  bayesx_logfile(abundance_west_full_mult_tr)
  summary(abundance_west_full_mult_tr)
}

#West model. Mult. Lag. TR.
{
  abundance_west_full_mult_lag_tr <- bayesx(PupaeMult ~ 
                                       
                                       #Random Effects
                                       sx(HouseID, by = Site, bs = 'ra', nrknots = 80) +
                                       
                                       #Space via geosplines 
                                       sx(HouseID, bs = "gs", map = all_bnd) +
                                       
                                       #Non-linear effects
                                       sx(Month, bs = "ps", nrknots = 5, degree = 3) +
                                       sx(Temp, Rain, bs = 'te', knots = 5) +
                                       sx(YearName, bs = 'ps') +
                                       
                                       #Linear Effects
                                       Housewall +
                                       Houseroof +
                                       Firewood +
                                       #Insmos +
                                       #Bednet +
                                       SleepersB +
                                       RoomsB + 
                                       Eaves +
                                       HC +
                                       Ceilings +
                                       Growth +
                                       Lag1 + Lag2 +
                                       Urban,
                                     
                                     family  = "cumlogit",
                                     method = "REML",
                                     #maxiter = 200,
                                     
                                     #Data.
                                     data = wes
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_west_full_mult_lag_tr, "Model Results/Abundance Model. West. Mult. Lag. TR.rds")
  bayesx_logfile(abundance_west_full_mult_lag_tr)
  summary(abundance_west_full_mult_lag_tr)
}

#West model. Mult. Lag. TR. STEP.
{
  abundance_west_full_mult_lag_tr_step <- bayesx(PupaeBin ~ 
                                              
                                              #Random Effects
                                              sx(HouseID, by = Site, bs = 'ra', nrknots = 80) +
                                              
                                              #Space via geosplines 
                                              sx(HouseID, bs = "gs", map = all_bnd) +
                                              
                                              #Non-linear effects
                                              sx(Month, bs = "ps", nrknots = 5, degree = 3) +
                                              sx(Temp, Rain, bs = 'te', knots = 5) +
                                              sx(YearName, bs = 'ps') +
                                              
                                              #Linear Effects
                                              Houseroof +
                                              Firewood +
                                              #Insmos +
                                              #Bednet +
                                              SleepersB +
                                              RoomsB + 
                                              Eaves +
                                              HC +
                                              Growth +
                                              Lag1 + 
                                              Lag2 +
                                              Urban,
                                            
                                            family  = "binomial",
                                            method = "REML",
                                            #maxiter = 200,
                                            
                                            #Data.
                                            data = wes
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_west_full_mult_lag_tr_step, "Model Results/Abundance Model. West. Mult. Lag. TR. STEP.rds")
  bayesx_logfile(abundance_west_full_mult_lag_tr_step)
  summary(abundance_west_full_mult_lag_tr_step)
}

#Residuals Binomial Models. Not Run.
#Full model. Low
{
  abundance_wes_bin_res_low <- bayesx(Low ~ 
                                         
                                         #Random Effects
                                         sx(HouseID, bs = 'ra', knots = 40) +
                                         sx(Site, bs = 'ra', knots = 2) +
                                         
                                         #Spatial Term
                                         sx(HouseID, bs = 'gs', map = all_bnd) +
                                         
                                         #Non-linear effects
                                         sx(Temp, Rain, bs = 'te', knots = 5, degree = 2) +
                                         sx(Month, bs = 'ps', knots = 5, degree = 3) +
                                         sx(YearName, bs = 'ps') +  
                                         
                                         #Linear Effects
                                         Housewall +
                                         Houseroof +
                                         Firewood +
                                         Insmos +
                                         RoomsB + 
                                         Eaves +
                                         Ceilings +
                                         SleepersB + 
                                         Growth +
                                         HC + 
                                         Location +
                                         Lag1 + Lag2 +
                                         Urban,
                                       
                                       #Model
                                       family = "binomial",
                                       method = "REML",
                                       #maxiter = 200,
                                       
                                       #Data
                                       data = abun
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_wes_bin_res_low, "Model Results/Abundance Model. West. Bin. Residuals. Low.rds")
  bayesx_logfile(abundance_wes_bin_res_low)
  summary(abundance_wes_bin_res_low)
}

#Full model. Int
{
  abundance_wes_bin_res_int <- bayesx(Int ~ 
                                        
                                        #Random Effects
                                        sx(HouseID, bs = 'ra', knots = 40) +
                                        sx(Site, bs = 'ra', knots = 2) +
                                        
                                        #Spatial Term
                                        sx(HouseID, bs = 'gs', map = all_bnd) +
                                        
                                        #Non-linear effects
                                        sx(Temp, Rain, bs = 'te', knots = 5, degree = 2) +
                                        sx(Month, bs = 'ps', knots = 5, degree = 3) +
                                        sx(YearName, bs = 'ps') +  
                                        
                                        #Linear Effects
                                        Housewall +
                                        Houseroof +
                                        Firewood +
                                        Insmos +
                                        RoomsB + 
                                        Eaves +
                                        Ceilings +
                                        SleepersB + 
                                        Growth +
                                        HC + 
                                        Location +
                                        Lag1 + Lag2 +
                                        Urban,
                                      
                                      #Model
                                      family = "binomial",
                                      method = "REML",
                                      #maxiter = 200,
                                      
                                      #Data
                                      data = abun
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_wes_bin_res_int, "Model Results/Abundance Model. West. Bin. Residuals. Int.rds")
  bayesx_logfile(abundance_wes_bin_res_int)
  summary(abundance_wes_bin_res_int)
}

#Full model. High
{
  abundance_wes_bin_res_hig <- bayesx(Hig ~ 
                                        
                                        #Random Effects
                                        sx(HouseID, bs = 'ra', knots = 40) +
                                        sx(Site, bs = 'ra', knots = 2) +
                                        
                                        #Spatial Term
                                        sx(HouseID, bs = 'gs', map = all_bnd) +
                                        
                                        #Non-linear effects
                                        sx(Temp, Rain, bs = 'te', knots = 5, degree = 2) +
                                        sx(Month, bs = 'ps', knots = 5, degree = 3) +
                                        sx(YearName, bs = 'ps') +  
                                        
                                        #Linear Effects
                                        Housewall +
                                        Houseroof +
                                        Firewood +
                                        Insmos +
                                        RoomsB + 
                                        Eaves +
                                        Ceilings +
                                        SleepersB + 
                                        Growth +
                                        HC + 
                                        Location +
                                        Lag1 + Lag2 +
                                        Urban,
                                      
                                      #Model
                                      family = "binomial",
                                      method = "REML",
                                      #maxiter = 200,
                                      
                                      #Data
                                      data = abun
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_wes_bin_res_hig, "Model Results/Abundance Model. West. Bin. Residuals. Hig.rds")
  bayesx_logfile(abundance_wes_bin_res_hig)
  summary(abundance_wes_bin_res_hig)
}

#West model. Poisson. 
{
  abundance_west_full <- bayesx(Pupae ~ 
                                  
                                  #Random Effects
                                  sx(HouseID, by = Site, bs = 'ra', nrknots = 80) +
                                  
                                  #Space via geosplines 
                                  sx(HouseID, bs = "gs", map = all_bnd) +
                                  
                                  #Non-linear effects
                                  sx(Habitatcount, bs = "ps", nrknots = 5) + 
                                  sx(Month, by = 'YearName', bs = "ps", nrknots = 5) +
                                  #sx(YearName, bs = "ps") + 
                                  
                                  #Linear Effects
                                  Housewall +
                                  Houseroof +
                                  Firewood +
                                  #Insmos +
                                  #Bednet +
                                  SleepersB +
                                  RoomsB + 
                                  Eaves +
                                  Ceilings +
                                  Growth +
                                  Urban,
                                
                                zipdistopt = "zinb",
                                family  = "poisson",
                                method = "REML",
                                maxiter = 200,
                                
                                #Data.
                                data = wes
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_west_full, "Model Results/Abundance Model. West. Full.rds")
  bayesx_logfile(abundance_west_full)
  summary(abundance_west_full)
}

#West Model with temp and rain. Poisson. 
{
  abundance_west_full_tr <- bayesx(Pupae ~ 
                                     
                                     #Random Effects
                                     sx(HouseID, by = Site, bs = 'ra', nrknots = 80) +
                                     
                                     #Space via geosplines 
                                     sx(HouseID, bs = "gs", map = all_bnd) +
                                     
                                     #Non-linear effects
                                     sx(Habitatcount, bs = "ps", nrknots = 5) + 
                                     sx(Temp, bs = 'ps', knots = 5) +
                                     sx(Rain, bs = 'ps', knots = 5) +                                  sx(YearName, bs = "ps") + 
                                     
                                     #Linear Effects
                                     Housewall +
                                     Houseroof +
                                     Firewood +
                                     #Insmos +
                                     #Bednet +
                                     SleepersB +
                                     RoomsB +
                                     Eaves +
                                     Ceilings +
                                     Growth +
                                     Urban,
                                   
                                   zipdistopt = "zinb",
                                   family  = "poisson",
                                   method = "REML",
                                   maxiter = 200,
                                   
                                   #Data.
                                   data = wes
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_west_full_tr, "Model Results/Abundance Model. West. Full. TR.rds")
  bayesx_logfile(abundance_west_full_tr)
  summary(abundance_west_full_tr)
}
#****************************************************************************************#
#****************************************************************************************#
#Individual Site models for Njenga's thesis. 
#Ukunda model. Mult. Month. TR. ##Main Model###
uku <- coa %>% 
  filter(Site == 'Ukunda')
{
  abundance_uku_mult_trm <- bayesx(PupaeMult ~ 
                                      
                                      #Random Effects
                                      sx(HouseID, bs = 'ra', knots = 20) +

                                      #Spatial Term
                                      sx(HouseID, bs = 'gs', map = all_bnd) +
                                      
                                      #Non-linear effects
                                      sx(Temp, bs = 'ps', knots = 5) +
                                      sx(Rain, bs = 'ps', knots = 5) +
                                      sx(MonthNum, by = YearNum, bs = 'season', period = 12) +
                                      sx(YearNum, bs = 'ps') +
                                      
                                      #Linear Effects
                                      Housewall +
                                      Houseroof +
                                      Firewood +
                                      #Insmos +
                                      RoomsB + 
                                      Eaves +
                                      Ceilings +
                                      SleepersB + 
                                      Growth +
                                      HC,# + 
                                      #Location +
                                      #Urban,
                                    
                                    family  = "cumlogit",
                                    method = "REML",
                                    
                                    #Data.
                                    data = uku
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_uku_mult_trm, "Model Results/Abundance Model. Uku. Mult. TRM.rds")
  bayesx_logfile(abundance_uku_mult_trm)
  summary(abundance_uku_mult_trm)
}

#Msambweni. 
msa <- coa %>% 
  filter(Site == 'Msambweni')
{
  abundance_msa_mult_trm <- bayesx(PupaeMult ~ 
                                     
                                     #Random Effects
                                     sx(HouseID, bs = 'ra', knots = 20) +
                                     
                                     #Spatial Term
                                     sx(HouseID, bs = 'gs', map = all_bnd) +
                                     
                                     #Non-linear effects
                                     sx(Temp, bs = 'ps', knots = 5) +
                                     sx(Rain, bs = 'ps', knots = 5) +
                                     sx(MonthNum, by = YearNum, bs = 'season', period = 12) +
                                     sx(YearNum, bs = 'ps') +
                                     
                                     #Linear Effects
                                     Housewall +
                                     Houseroof +
                                     Firewood +
                                     #Insmos +
                                     RoomsB + 
                                     Eaves +
                                     Ceilings +
                                     SleepersB + 
                                     Growth +
                                     HC,# + 
                                   #Location +
                                   #Urban,
                                   
                                   family  = "cumlogit",
                                   method = "REML",
                                   
                                   #Data.
                                   data = msa
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_msa_mult_trm, "Model Results/Abundance Model. Msa. Mult. TRM.rds")
  bayesx_logfile(abundance_msa_mult_trm)
  summary(abundance_msa_mult_trm)
  }
#****************************************************************************************#
#****************************************************************************************#
#Model Set 3: Abundance Coast Models.
#Coast Final Models. 
{
  abundance_coa_mult_trm <- bayesx(PupaeMult ~ 
                                      
                                      #Random Effects
                                      sx(HouseID, bs = 'ra', knots = 40) +
                                      sx(Site, bs = 'ra', knots = 2) +
                                      
                                      #Spatial Term
                                      sx(HouseID, bs = 'gs', map = all_bnd) +
                                      
                                      #Non-linear effects
                                      sx(Temp, bs = 'ps', knots = 5) +
                                      sx(Rain, bs = 'ps', knots = 5) +
                                      sx(MonthNum, by = YearNum, bs = 'season', period = 12) +
                                      sx(YearNum, bs = 'ps') +
                                        
                                      #Linear Effects
                                      Housewall +
                                      Houseroof +
                                      Firewood +
                                      #Insmos +
                                      RoomsB + 
                                      Eaves +
                                      Ceilings +
                                      SleepersB + 
                                      Growth +
                                      HC + 
                                      #Location +
                                      Urban,
                                    
                                    family  = "cumlogit",
                                    method = "REML",
                                    
                                    #Data.
                                    data = coa
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_coa_mult_trm, "Model Results/Abundance Model. Coast. Mult. TRM.rds")
  bayesx_logfile(abundance_coa_mult_trm)
  summary(abundance_coa_mult_trm)
}

#Coast. Mult. 
{
  abundance_coast_full_mult <- bayesx(PupaeMult ~ 
                                   
                                   #Random Effects
                                   sx(HouseID, by = Site, bs = 'ra', nrknots = 80) +
                                   
                                   #Spatial
                                   sx(HouseID, bs = "gs", map = all_bnd) +
                                   
                                   #Non-linear effects
                                   sx(Month, bs = "ps", nrknots = 5, degree = 3) +
                                   sx(YearName, bs = 'ps') +

                                   #Linear Effects
                                   Housewall +
                                   Houseroof +
                                   Firewood +
                                   Insmos +
                                   RoomsB + 
                                   Eaves +
                                   Ceilings +
                                   SleepersB + 
                                   Growth + 
                                   HC, 
                                   #Urban,
                                 
                                 #Model
                                 family = "cumlogit",
                                 method = "REML",
                                 #maxiter = 200,
                                 
                                 #Data. 
                                 data = coa
  )
  
  #####
  #Converged with no outliers.
  #####
  
  #Some Outputs.
  saveRDS(abundance_coast_full_mult, "Model Results/Abundance Model. Coast. Full. Mult.rds")
  bayesx_logfile(abundance_coast_full_mult)
  summary(abundance_coast_full_mult)
}

#Coast. Mult. TR.
{
  abundance_coast_full_mult_tr <- bayesx(PupaeMult ~ 
                                        
                                        #Random Effects
                                        sx(HouseID, by = Site, bs = 'ra', nrknots = 80) +
                                        
                                        #Spatial
                                        sx(HouseID, bs = "gs", map = all_bnd) +
                                        
                                        #Non-linear effects
                                        sx(Temp, bs = 'ps', knots = 5) +
                                        sx(Rain, bs = 'ps', knots = 5) +
                                        sx(YearName, bs = 'ps') +
                                        
                                        #Linear Effects
                                        Housewall +
                                        Houseroof +
                                        Firewood +
                                        Insmos +
                                        RoomsB + 
                                        Eaves +
                                        HC + 
                                        Ceilings +
                                        SleepersB + 
                                        Growth, 
                                      #Urban,
                                      
                                      #Model
                                      family = "cumlogit",
                                      method = "REML",
                                      #maxiter = 200,
                                      
                                      #Data. 
                                      data = coa
  )
  
  #####
  #Converged with no outliers.
  #####
  
  #Some Outputs.
  saveRDS(abundance_coast_full_mult_tr, "Model Results/Abundance Model. Coast. Full. Mult. TR.rds")
  bayesx_logfile(abundance_coast_full_mult_tr)
  summary(abundance_coast_full_mult_tr)
}

#Coast. Mult. Lag.
{
  abundance_coast_full_mult_lag <- bayesx(PupaeMult ~ 
                                        
                                        #Random Effects
                                        sx(HouseID, by = Site, bs = 'ra', nrknots = 80) +
                                        
                                        #Spatial
                                        sx(HouseID, bs = "gs", map = all_bnd) +
                                        
                                        #Non-linear effects
                                        sx(Month, bs = "ps", nrknots = 5, degree = 3) +
                                        sx(YearName, bs = 'ps') +
                                        
                                        #Linear Effects
                                        Housewall +
                                        Houseroof +
                                        Firewood +
                                        Insmos +
                                        RoomsB + 
                                        Eaves +
                                        Ceilings +
                                        SleepersB + 
                                        Growth + 
                                        HC +
                                        Lag1 + Lag2, 
                                        #Urban,
                                      
                                      #Model
                                      family = "cumlogit",
                                      method = "REML",
                                      #maxiter = 200,
                                      
                                      #Data. 
                                      data = coa
  )
  
  #####
  #Converged with no outliers.
  #####
  
  #Some Outputs.
  saveRDS(abundance_coast_full_mult_lag, "Model Results/Abundance Model. Coast. Full. Mult. Lag.rds")
  bayesx_logfile(abundance_coast_full_mult_lag)
  summary(abundance_coast_full_mult_lag)
}

#Residuals Binomial Models. Not Run.
#Full model. Low
{
  abundance_coa_bin_res_low <- bayesx(Low ~ 
                                        
                                        #Random Effects
                                        sx(HouseID, bs = 'ra', knots = 40) +
                                        sx(Site, bs = 'ra', knots = 2) +
                                        
                                        #Spatial Term
                                        sx(HouseID, bs = 'gs', map = all_bnd) +
                                        
                                        #Non-linear effects
                                        sx(Temp, Rain, bs = 'te', knots = 5, degree = 2) +
                                        sx(Month, bs = 'ps', knots = 5, degree = 3) +
                                        sx(YearName, bs = 'ps') +  
                                        
                                        #Linear Effects
                                        Housewall +
                                        Houseroof +
                                        Firewood +
                                        Insmos +
                                        RoomsB + 
                                        Eaves +
                                        Ceilings +
                                        SleepersB + 
                                        Growth +
                                        HC + 
                                        Location +
                                        Lag1 + Lag2 +
                                        Urban,
                                      
                                      #Model
                                      family = "binomial",
                                      method = "REML",
                                      #maxiter = 200,
                                      
                                      #Data
                                      data = abun
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_coa_bin_res_low, "Model Results/Abundance Model. Coast. Bin. Residuals. Low.rds")
  bayesx_logfile(abundance_coa_bin_res_low)
  summary(abundance_coa_bin_res_low)
}

#Full model. Int
{
  abundance_coa_bin_res_int <- bayesx(Int ~ 
                                        
                                        #Random Effects
                                        sx(HouseID, bs = 'ra', knots = 40) +
                                        sx(Site, bs = 'ra', knots = 2) +
                                        
                                        #Spatial Term
                                        sx(HouseID, bs = 'gs', map = all_bnd) +
                                        
                                        #Non-linear effects
                                        sx(Temp, Rain, bs = 'te', knots = 5, degree = 2) +
                                        sx(Month, bs = 'ps', knots = 5, degree = 3) +
                                        sx(YearName, bs = 'ps') +  
                                        
                                        #Linear Effects
                                        Housewall +
                                        Houseroof +
                                        Firewood +
                                        Insmos +
                                        RoomsB + 
                                        Eaves +
                                        Ceilings +
                                        SleepersB + 
                                        Growth +
                                        HC + 
                                        Location +
                                        Lag1 + Lag2 +
                                        Urban,
                                      
                                      #Model
                                      family = "binomial",
                                      method = "REML",
                                      #maxiter = 200,
                                      
                                      #Data
                                      data = abun
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_coa_bin_res_int, "Model Results/Abundance Model. Coast. Bin. Residuals. Int.rds")
  bayesx_logfile(abundance_coa_bin_res_int)
  summary(abundance_coa_bin_res_int)
}

#Full model. High
{
  abundance_coa_bin_res_hig <- bayesx(Hig ~ 
                                        
                                        #Random Effects
                                        sx(HouseID, bs = 'ra', knots = 40) +
                                        sx(Site, bs = 'ra', knots = 2) +
                                        
                                        #Spatial Term
                                        sx(HouseID, bs = 'gs', map = all_bnd) +
                                        
                                        #Non-linear effects
                                        sx(Temp, Rain, bs = 'te', knots = 5, degree = 2) +
                                        sx(Month, bs = 'ps', knots = 5, degree = 3) +
                                        sx(YearName, bs = 'ps') +  
                                        
                                        #Linear Effects
                                        Housewall +
                                        Houseroof +
                                        Firewood +
                                        Insmos +
                                        RoomsB + 
                                        Eaves +
                                        Ceilings +
                                        SleepersB + 
                                        Growth +
                                        HC + 
                                        Location +
                                        Lag1 + Lag2 +
                                        Urban,
                                      
                                      #Model
                                      family = "binomial",
                                      method = "REML",
                                      #maxiter = 200,
                                      
                                      #Data
                                      data = abun
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_coa_bin_res_hig, "Model Results/Abundance Model. Coast. Bin. Residuals. Hig.rds")
  bayesx_logfile(abundance_coa_bin_res_hig)
  summary(abundance_coa_bin_res_hig)
}

#Coast. Poisson. 
{
  coa$Pupae[coa$Pupae > 200] <- NA
  abundance_coast_full <- bayesx(Pupae ~ 
                                   
                                   #Random Effects
                                   sx(HouseID, by = Site, bs = 'ra') +
                                   
                                   #Spatial
                                   sx(HouseID, bs = "gs", map = all_bnd) +
                                   
                                   #Non-linear effects
                                   sx(Habitatcount, bs = "ps", nrknots = 5) + 
                                   sx(Month, bs = "ps", nrknots = 5) +
                                   sx(YearName, bs = "ps") + 
                                   
                                   #Linear Effects
                                   Housewall +
                                   Houseroof +
                                   Firewood +
                                   Insmos +
                                   Bednet +
                                   RoomsB + 
                                   Eaves +
                                   Ceilings +
                                   SleepersB + 
                                   Growth +
                                   Urban,
                                 
                                 #Model
                                 zipdistopt = "zinb",
                                 family = "poisson",
                                 method = "REML",
                                 maxiter = 400,
                                 
                                 #Data. 
                                 data = coa
  )
  
  #####
  #Converged with no outliers.
  #####
  
  #Some Outputs.
  saveRDS(abundance_coast_full, "Model Results/Abundance Model. Coast. Full.rds")
  bayesx_logfile(abundance_coast_full)
  summary(abundance_coast_full)
}

#Coast with temp and rain. Poisson. 
{
  
  abundance_coast_full_tr <- bayesx(Pupae ~ 
                                      
                                      #Random Effects
                                      sx(HouseID, by = Site, bs = 'ra') +
                                      
                                      #Spatial
                                      sx(HouseID, bs = "gs", map = all_bnd) +
                                      
                                      #Non-linear effects
                                      sx(Habitatcount, bs = "ps", nrknots = 5) + 
                                      sx(Temp, bs = 'ps', knots = 5) +
                                      sx(Rain, bs = 'ps', knots = 5) +                                         sx(YearName, bs = "ps") + 
                                      
                                      #Linear Effects
                                      Housewall +
                                      Houseroof +
                                      Firewood +
                                      Insmos +
                                      Bednet +
                                      RoomsB + 
                                      Eaves +
                                      Ceilings +
                                      SleepersB + 
                                      Growth +
                                      Urban,
                                    
                                    #Model
                                    zipdistopt = "zinb",
                                    family = "poisson",
                                    method = "REML",
                                    maxiter = 200,
                                    
                                    #Data. 
                                    data = coa
  )
  
  #####
  #Converges without outliers. 
  #####
  
  #Some Outputs.
  saveRDS(abundance_coast_full_tr, "Model Results/Abundance Model. Coast. Full. TR.rds")
  bayesx_logfile(abundance_coast_full_tr)
  summary(abundance_coast_full_tr)
}
#****************************************************************************************#

#****************************************************************************************#
#Model Set 4: Persistence Full Models.
{
  persistence_full <- bayesx(Persistent ~ 
                               
                               #Random Effects
                               sx(HouseID,  bs = 'ra', knots = 80) +
                               sx(Site,  bs = 'ra', knots = 4) +
                               
                               sx(HouseID, bs = 'gs', map = all_bnd) +
                               
                               #Non-linear effects
                               sx(YearNum, bs = 'ps') +
                               
                               #Linear Effects
                               Housewall +
                               Houseroof +
                               Firewood +
                               #Insmos + Uninformative. The Category has zero cells. 
                               RoomsB + 
                               Eaves +
                               Ceilings +
                               SleepersB + 
                               Growth + 
                               Urban +
                               Location + 
                               HC,
                             
                             family = "binomial",
                             method = "REML",

                             #Data.
                             data = pers
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(persistence_full, "Model Results/Persistence Model. Full.rds")
  bayesx_logfile(persistence_full)
  summary(persistence_full)
}
#****************************************************************************************#

#****************************************************************************************#
#Sensitivity Analysis. Repeat the model excluding any replacement households. 
#Full model. Mult 
#1. Remove household replacement data. 
{
  sa_abun <- abun[-which(abun$Site == 'Msambweni'),]
  abundance_full_mult_sa <- bayesx(PupaeMult ~ 
                                     
                                     #Random Effects
                                     sx(HouseID, bs = 'ra', knots = 60) +
                                     sx(Site, bs = 'ra', knots = 3) +
                                     
                                     #Spatial Term
                                     sx(HouseID, bs = 'gs', map = all_bnd) +
                                     
                                     #Non-linear effects
                                     sx(Temp, bs = 'ps', knots = 5) +
                                     sx(Rain, bs = 'ps', knots = 5) +
                                     sx(MonthNum, by = YearNum, bs = 'season', period = 12) +
                                     sx(YearNum, bs = 'ps', knots = 5) +
                                     
                                     #Linear Effects
                                     Housewall +
                                     Houseroof +
                                     Firewood +
                                     Insmos +
                                     RoomsB + 
                                     Eaves +
                                     Ceilings +
                                     SleepersB + 
                                     Growth +
                                     HC + 
                                     Location +
                                     Urban,
                                   
                                   #Model
                                   family = "cumlogit",
                                   method = "REML",
                                   
                                   #Data
                                   data = sa_abun
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_full_mult_sa, "Model Results/Abundance Model. Full. Mult. SA.rds")
  bayesx_logfile(abundance_full_mult_sa)
  summary(abundance_full_mult_sa)
}

#2. Remove imputed  data. Not run. 
{
  sa_abun <- abun[-which(abun$Site == 'Msambweni'),]
  abundance_full_mult_sa <- bayesx(PupaeMult ~ 
                                     
                                     #Random Effects
                                     sx(HouseID, by = Site, bs = 'ra', knots = 60) +
                                     
                                     #Spatial Term
                                     sx(HouseID, bs = 'gs', map = all_bnd) +
                                     
                                     #Non-linear effects
                                     sx(MonthNum, by = YearNum, bs = 'season', period = 12) +
                                     sx(YearName, bs = 'ps') +  
                                     
                                     #Linear Effects
                                     Housewall +
                                     Houseroof +
                                     Firewood +
                                     Insmos +
                                     RoomsB + 
                                     Eaves +
                                     Ceilings +
                                     SleepersB + 
                                     Growth +
                                     HC + 
                                     Location +
                                     Urban,
                                   
                                   #Model
                                   family = "cumlogit",
                                   method = "REML",
                                   #maxiter = 200,
                                   
                                   #Data
                                   data = sa_abun
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abundance_full_mult_sa, "Model Results/Abundance Model. Full. Mult. SA.rds")
  bayesx_logfile(abundance_full_mult_sa)
  summary(abundance_full_mult_sa)
}

#3. Remove outliers replacement data. 
{
  
  sa_outliers <- readRDS("Data Files/VAbundance. Residuals.rds")
  sa_outliers$HouseID <- as.factor(sa_outliers$HouseID)
  sa_outliers$Season <- as.factor(sa_outliers$Season)
  abun_mult_sa_outliers <- bayesx(PupaeMult ~ 
                                      
                                      #Random Effects
                                      sx(HouseID, bs = 'ra', knots = 80) +
                                      sx(Site, bs = 'ra', knots = 4) +
                                      
                                      #Spatial Term
                                      sx(HouseID, bs = 'gs', map = all_bnd) +
                                      
                                      #Non-linear effects
                                      sx(Temp, bs = 'ps', knots = 5) +
                                      sx(Rain, bs = 'ps', knots = 5) +
                                      sx(MonthNum, by = YearNum, bs = 'season', period = 12) +
                                      sx(YearName, bs = 'ps') +  
                                    
                                      
                                      #Linear Effects
                                      Housewall +
                                      Houseroof +
                                      Firewood +
                                      Insmos +
                                      RoomsB + 
                                      Eaves +
                                      Ceilings +
                                      SleepersB + 
                                      Growth +
                                      HC + 
                                      Location +
                                      Urban,
                                    
                                    #Model
                                    family = "cumlogit",
                                    method = "REML",
                                    #maxiter = 200,
                                    
                                    #Data
                                    data = sa_outliers
  )
  
  #####
  #Converged
  #####
  
  #Some Outputs.
  saveRDS(abun_mult_sa_outliers, "Model Results/Abundance Model. Mult. SA. Outliers.rds")
  bayesx_logfile(abun_mult_sa_outliers)
  summary(abun_mult_sa_outliers)
}
#The End. 
#****************************************************************************************#

#****************************************************************************************#