# Computing chill delay for three cultivars representative of the three phases
# Required libraries
library(chillR)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(patchwork)
library(cowplot)
library(reshape2)
library(kableExtra)
library(ggpmisc)
library(colorRamps)

# Upload functions from helper functions by Fernandez
source("0_helper_functions.R")

# Upload fitted Olive PhenoFlex model
Pheno_fit <- readRDS("PhenFlex_olive.rds")

# Chill delay for low chill cultivars (Sabatera (1-22)) ####

# End of chill for observed temperatures ####
# Hourly temperatures
temp_df <- stack_hourly_temps(tassaout_weather, latitude = 31.82)

# Primer data to save the output
historic_endchill <- temp_df$hourtemps %>% group_by(Year) %>% summarise(scen_year="Historic",Endodormancy = NA)

# Define the seasons to be used for predicting the phenology
iSeason <- genSeason(temp_df$hourtemps, mrange = c(11, 6), years = c(1973 : 2022))

# Change the names of the list of seasons to be used as index in the next for loop
names(iSeason) <- 1973 : 2022

# Implement a for loop that runs over the list of seasons to estimate the endodormancy date using the parameters fitted by the
# model
for (i in 1 : length(iSeason)){
  
  num_rows <- length(iSeason[[i]])
  
  season_data <-temp_df$hourtemps[iSeason[[i]][1464:num_rows],]
  
  res <- PhenoFlex(temp=season_data$Temp,
                   times=c(1: length(season_data$Temp)),
                   stopatzc=T,basic_output=F,
                   yc=32.77, 
                   zc=845, 
                   s1=7.992534e-01, 
                   Tu=2.681513e+01,
                   E0=3.240997e+03, 
                   E1=9.921809e+03,
                   A0=6.867300e+03, 
                   A1=5.939913e+13,
                   Tf=7.492625e+00, 
                   Tc=3.427934e+01, 
                   Tb=2.959986e+00, 
                   slope=3.555938e+00)
  
  season_data[,"y"]<-res$y
  season_data<-add_date(season_data)
  
  CR_full<-season_data$JDay[which(season_data$y>=32.77)[i]]
  
  # Add the endodormancy date to the primer data set
  historic_endchill[historic_endchill$Year == names(iSeason)[i], "Endodormancy"] <- CR_full
}
Historic_end <- median(historic_endchill$Endodormancy,na.rm = TRUE)
historic_endchill[,"Historic_end"]<- Historic_end
historic_endchill[,"Diff"]<- historic_endchill$Endodormancy-historic_endchill$Historic_end

# End of chill for past simulated scenarios ####
# Load the historic simulated scenarios
temps_past_scenarios <- load_temperature_scenarios("Weather/", "Tassaout_historic")

# Create a single dataset for all the simulated years
temps_past_scenarios <- bind_rows(temps_past_scenarios, .id = "Scen_year")

# Make a primer dataset to allocate the results of the endchill projection
simulated_endchill <- temps_past_scenarios %>% group_by(Scen_year, Year) %>% summarise(Endodormancy = NA)

# Define the scenario years to be used in the loop
scen_years <- unique(simulated_endchill$Scen_year)

# Implement the for loop
for (scen_year in scen_years){
  
  # Subset a temporary data frame according to scenario and climate model
  temp_df <- filter(temps_past_scenarios, Scen_year == scen_year)
  
  # Generate hourly temperatures in the temporary data frame
  temp_df <- stack_hourly_temps(temp_df, latitude = 31.82)
  
  # Define the seasons to be used for predicting the phenology
  iSeason <- genSeason(temp_df$hourtemps, mrange = c(11, 6), years = c(2002 : 2101))
  
  # Change the names of the list of seasons to be used as index in the next for loop
  names(iSeason) <- 2002 : 2101
  
  # Implement a for loop that runs over the list of seasons to estimate the endodormancy date using the parameters fitted by the
  # model
  for (i in 1 : length(iSeason)){
    
    num_rows <- length(iSeason[[i]])
    
    season_data <-temp_df$hourtemps[iSeason[[i]][1464:num_rows],]
    
    res <- PhenoFlex(temp=season_data$Temp,
                     times=c(1: length(season_data$Temp)),
                     stopatzc=T,basic_output=F,
                     yc=32.77, 
                     zc=845,
                     s1=7.992534e-01, 
                     Tu=2.681513e+01,
                     E0=3.240997e+03, 
                     E1=9.921809e+03,
                     A0=6.867300e+03, 
                     A1=5.939913e+13,
                     Tf=7.492625e+00, 
                     Tc=3.427934e+01, 
                     Tb=2.959986e+00, 
                     slope=3.555938e+00)
    
    season_data[,"y"]<-res$y
    season_data<-add_date(season_data)
    
    CR_full<-season_data$JDay[which(season_data$y>=32.77)[i]]
    
    # Add the endodormancy date to the primer data set
    simulated_endchill[simulated_endchill$Scen_year == scen_year & 
                         simulated_endchill$Year == names(iSeason)[i], "Endodormancy"] <- CR_full
  }
}
simulated_endchill[,"Historic_end"] <- Historic_end
simulated_endchill[,"Diff"]<- simulated_endchill$Endodormancy-simulated_endchill$Historic_end

# End of chill for future scenarios ####
future_temps <- list("Tassaout_2050_rcp45" = load_temperature_scenarios("Weather", "Tassaout_2050_rcp45"),
                     "Tassaout_2050_rcp85" = load_temperature_scenarios("Weather", "Tassaout_2050_rcp85"),
                     "Tassaout_2085_rcp45" = load_temperature_scenarios("Weather", "Tassaout_2085_rcp45"),
                     "Tassaout_2085_rcp85" = load_temperature_scenarios("Weather", "Tassaout_2085_rcp85"))

# Combine the datasets of each list into a single dataset per scenario
future_temps <- data.frame(
  bind_rows(
    lapply(future_temps, function (x) bind_rows(x, .id = "GCM")), .id = "List"))

# Re-structure the data. Add the RCP, Time and RCP_Time columns
future_temps <- future_temps %>%
  mutate(RCP = if_else(substr(.$List, 15, 20) == "rcp45", "RCP4.5", "RCP8.5"),
         Time = substr(.$List, 10, 13)) %>% 
  select(DATE, Year, Month, Day, Tmin, Tmax, RCP, Time, GCM) %>% 
  mutate(RCP_Time = paste0(.$RCP, "_", .$Time))

# Create a primer data frame to allocate the results for future endodormancy
future_endchill <- future_temps %>% group_by(RCP_Time, GCM, Year) %>% summarise(Endodormancy = NA)

# Define the scenarios to be used in the for loop
scenarios <- unique(future_temps$RCP_Time)

# Define the climate models to be used in the for loop
climate_models <- unique(future_temps$GCM)

# Implement the for loop
for (scenario in scenarios){
  for (climate_model in climate_models){
    
    # Subset a temporary data frame according to scenario and climate model
    temp_df <- filter(future_temps, RCP_Time == scenario & GCM == climate_model)
    
    # Generate hourly temperatures in the temporary data frame
    temp_df <- stack_hourly_temps(temp_df, latitude = 31.82)
    
    # Define the seasons to be used for predicting the phenology
    iSeason <- genSeason(temp_df$hourtemps, mrange = c(11, 6), years = c(2002 : 2101))
    
    # Change the names of the list of seasons to be used as index in the next for loop
    names(iSeason) <- 2002 : 2101
    
    for (i in 1 : length(iSeason)){
      
      num_rows <- length(iSeason[[i]])
      
      season_data <-temp_df$hourtemps[iSeason[[i]][1464:num_rows],]
      
      res <- PhenoFlex(temp=season_data$Temp,
                       times=c(1: length(season_data$Temp)),
                       stopatzc=T,basic_output=F,
                       yc=32.77, 
                       zc=845,
                       s1=7.992534e-01, 
                       Tu=2.681513e+01,
                       E0=3.240997e+03, 
                       E1=9.921809e+03,
                       A0=6.867300e+03, 
                       A1=5.939913e+13,
                       Tf=7.492625e+00, 
                       Tc=3.427934e+01, 
                       Tb=2.959986e+00, 
                       slope=3.555938e+00)
      
      season_data[,"y"]<-res$y
      season_data<-add_date(season_data)
      
      CR_full<-season_data$JDay[which(season_data$y>=32.77)[i]]
      
      # Add the bloom date to the primer data set
      future_endchill[future_endchill$RCP_Time == scenario & 
                        future_endchill$GCM == climate_model & 
                        future_endchill$Year == names(iSeason)[i],"Endodormancy"] <- CR_full
    }
  }
}

colnames(future_endchill)[colnames(future_endchill) == "Year"] <- "End_year"
future_endchill[,"Historic_end"]<- Historic_end
future_endchill[,"Diff"]<- future_endchill$Endodormancy-future_endchill$Historic_end
future_endchill <- separate(future_endchill, col = RCP_Time, into = c("RCP", "Year"), sep = "_")

# Put data together and plot everything in one plot with ggplot2
low_endchill_hist <- historic_endchill
low_endchill_simulated <- simulated_endchill
low_endchill_future  <- future_endchill

Low_chill_delay <- plot_lowchill_delay(low_endchill_hist,low_endchill_simulated,low_endchill_future,
                                    metric = "Diff",
                                    axis_label = "")


# Chill delay for Mid_chill cultivars (Picholine Marocaine (365-52)) ####

# End of chill for observed temperatures ####
# Hourly temperatures
temp_df <- stack_hourly_temps(tassaout_weather, latitude = 31.82)

# Primer data to save the output
historic_endchill <- temp_df$hourtemps %>% group_by(Year) %>% summarise(scen_year="Historic",Endodormancy = NA)

# Define the seasons to be used for predicting the phenology
iSeason <- genSeason(temp_df$hourtemps, mrange = c(11, 6), years = c(1973 : 2022))

# Change the names of the list of seasons to be used as index in the next for loop
names(iSeason) <- 1973 : 2022

# Implement a for loop that runs over the list of seasons to estimate the endodormancy date using the parameters fitted by the
# model
for (i in 1 : length(iSeason)){
  
  num_rows <- length(iSeason[[i]])
  
  season_data <-temp_df$hourtemps[iSeason[[i]][1464:num_rows],]
  
  res <- PhenoFlex(temp=season_data$Temp,
                   times=c(1: length(season_data$Temp)),
                   stopatzc=T,basic_output=F,
                   yc=5.800000e+01, 
                   zc=7.850000e+02, 
                   s1=7.992534e-01, 
                   Tu=2.681513e+01,
                   E0=3.240997e+03, 
                   E1=9.921809e+03,
                   A0=6.867300e+03, 
                   A1=5.939913e+13,
                   Tf=7.492625e+00, 
                   Tc=3.427934e+01, 
                   Tb=2.959986e+00, 
                   slope=3.555938e+00)
  
  season_data[,"y"]<-res$y
  season_data<-add_date(season_data)
  
  CR_full<-season_data$JDay[which(season_data$y>=5.800000e+01)[i]]
  
  # Add the endodormancy date to the primer data set
  historic_endchill[historic_endchill$Year == names(iSeason)[i], "Endodormancy"] <- CR_full
}
Historic_end <- median(historic_endchill$Endodormancy,na.rm = TRUE)
historic_endchill[,"Historic_end"]<- Historic_end
historic_endchill[,"Diff"]<- historic_endchill$Endodormancy-historic_endchill$Historic_end

# End of chill for past simulated scenarios ####
# Load the historic simulated scenarios
temps_past_scenarios <- load_temperature_scenarios("Weather/", "Tassaout_historic")

# Create a single dataset for all the simulated years
temps_past_scenarios <- bind_rows(temps_past_scenarios, .id = "Scen_year")

# Make a primer dataset to allocate the results of the endchill projection
simulated_endchill <- temps_past_scenarios %>% group_by(Scen_year, Year) %>% summarise(Endodormancy = NA)

# Define the scenario years to be used in the loop
scen_years <- unique(simulated_endchill$Scen_year)

# Implement the for loop
for (scen_year in scen_years){
  
  # Subset a temporary data frame according to scenario and climate model
  temp_df <- filter(temps_past_scenarios, Scen_year == scen_year)
  
  # Generate hourly temperatures in the temporary data frame
  temp_df <- stack_hourly_temps(temp_df, latitude = 31.82)
  
  # Define the seasons to be used for predicting the phenology
  iSeason <- genSeason(temp_df$hourtemps, mrange = c(11, 6), years = c(2002 : 2101))
  
  # Change the names of the list of seasons to be used as index in the next for loop
  names(iSeason) <- 2002 : 2101
  
  # Implement a for loop that runs over the list of seasons to estimate the endodormancy date using the parameters fitted by the
  # model
  for (i in 1 : length(iSeason)){
    
    num_rows <- length(iSeason[[i]])
    
    season_data <-temp_df$hourtemps[iSeason[[i]][1464:num_rows],]
    
    res <- PhenoFlex(temp=season_data$Temp,
                     times=c(1: length(season_data$Temp)),
                     stopatzc=T,basic_output=F,
                     yc=5.800000e+01, 
                     zc=7.850000e+02,
                     s1=7.992534e-01, 
                     Tu=2.681513e+01,
                     E0=3.240997e+03, 
                     E1=9.921809e+03,
                     A0=6.867300e+03, 
                     A1=5.939913e+13,
                     Tf=7.492625e+00, 
                     Tc=3.427934e+01, 
                     Tb=2.959986e+00, 
                     slope=3.555938e+00)
    
    season_data[,"y"]<-res$y
    season_data<-add_date(season_data)
    
    CR_full<-season_data$JDay[which(season_data$y>=5.800000e+01)[i]]
    
    # Add the endodormancy date to the primer data set
    simulated_endchill[simulated_endchill$Scen_year == scen_year & 
                         simulated_endchill$Year == names(iSeason)[i], "Endodormancy"] <- CR_full
  }
}
simulated_endchill[,"Historic_end"] <- Historic_end
simulated_endchill[,"Diff"]<- simulated_endchill$Endodormancy-simulated_endchill$Historic_end

# End of chill for future scenarios ####
future_temps <- list("Tassaout_2050_rcp45" = load_temperature_scenarios("Weather", "Tassaout_2050_rcp45"),
                     "Tassaout_2050_rcp85" = load_temperature_scenarios("Weather", "Tassaout_2050_rcp85"),
                     "Tassaout_2085_rcp45" = load_temperature_scenarios("Weather", "Tassaout_2085_rcp45"),
                     "Tassaout_2085_rcp85" = load_temperature_scenarios("Weather", "Tassaout_2085_rcp85"))

# Combine the datasets of each list into a single dataset per scenario
future_temps <- data.frame(
  bind_rows(
    lapply(future_temps, function (x) bind_rows(x, .id = "GCM")), .id = "List"))

# Re-structure the data. Add the RCP, Time and RCP_Time columns
future_temps <- future_temps %>%
  mutate(RCP = if_else(substr(.$List, 15, 20) == "rcp45", "RCP4.5", "RCP8.5"),
         Time = substr(.$List, 10, 13)) %>% 
  select(DATE, Year, Month, Day, Tmin, Tmax, RCP, Time, GCM) %>% 
  mutate(RCP_Time = paste0(.$RCP, "_", .$Time))

# Create a primer data frame to allocate the results for future endodormancy
future_endchill <- future_temps %>% group_by(RCP_Time, GCM, Year) %>% summarise(Endodormancy = NA)

# Define the scenarios to be used in the for loop
scenarios <- unique(future_temps$RCP_Time)

# Define the climate models to be used in the for loop
climate_models <- unique(future_temps$GCM)

# Implement the for loop
for (scenario in scenarios){
  for (climate_model in climate_models){
    
    # Subset a temporary data frame according to scenario and climate model
    temp_df <- filter(future_temps, RCP_Time == scenario & GCM == climate_model)
    
    # Generate hourly temperatures in the temporary data frame
    temp_df <- stack_hourly_temps(temp_df, latitude = 31.82)
    
    # Define the seasons to be used for predicting the phenology
    iSeason <- genSeason(temp_df$hourtemps, mrange = c(11, 6), years = c(2002 : 2101))
    
    # Change the names of the list of seasons to be used as index in the next for loop
    names(iSeason) <- 2002 : 2101
    
    for (i in 1 : length(iSeason)){
      
      num_rows <- length(iSeason[[i]])
      
      season_data <-temp_df$hourtemps[iSeason[[i]][1464:num_rows],]
      
      res <- PhenoFlex(temp=season_data$Temp,
                       times=c(1: length(season_data$Temp)),
                       stopatzc=T,basic_output=F,
                       yc=5.800000e+01, 
                       zc=7.850000e+02,
                       s1=7.992534e-01, 
                       Tu=2.681513e+01,
                       E0=3.240997e+03, 
                       E1=9.921809e+03,
                       A0=6.867300e+03, 
                       A1=5.939913e+13,
                       Tf=7.492625e+00, 
                       Tc=3.427934e+01, 
                       Tb=2.959986e+00, 
                       slope=3.555938e+00)
      
      season_data[,"y"]<-res$y
      season_data<-add_date(season_data)
      
      CR_full<-season_data$JDay[which(season_data$y>=5.800000e+01)[i]]
      
      # Add the bloom date to the primer data set
      future_endchill[future_endchill$RCP_Time == scenario & 
                        future_endchill$GCM == climate_model & 
                        future_endchill$Year == names(iSeason)[i],"Endodormancy"] <- CR_full
    }
  }
}

colnames(future_endchill)[colnames(future_endchill) == "Year"] <- "End_year"
future_endchill[,"Historic_end"]<- Historic_end
future_endchill[,"Diff"]<- future_endchill$Endodormancy-future_endchill$Historic_end
future_endchill <- separate(future_endchill, col = RCP_Time, into = c("RCP", "Year"), sep = "_")

# Put data together and plot everything in one plot with ggplot2
mid_endchill_hist <- historic_endchill
mid_endchill_simulated <- simulated_endchill
mid_endchill_future  <- future_endchill

Mid_chill_delay <- plot_midchill_delay(mid_endchill_hist,mid_endchill_simulated,mid_endchill_future,
                                     metric = "Diff",
                                     axis_label = "Chill delay (in days)") &
  theme(legend.position = "none")

# Chill delay for High chill cultivars (Leccino (325-55)) ####

# End of chill for observed temperatures ####
# Hourly temperatures
temp_df <- stack_hourly_temps(tassaout_weather, latitude = 31.82)

# Primer data to save the output
historic_endchill <- temp_df$hourtemps %>% group_by(Year) %>% summarise(scen_year="Historic",Endodormancy = NA)

# Define the seasons to be used for predicting the phenology
iSeason <- genSeason(temp_df$hourtemps, mrange = c(11, 6), years = c(1973 : 2022))

# Change the names of the list of seasons to be used as index in the next for loop
names(iSeason) <- 1973 : 2022

# Implement a for loop that runs over the list of seasons to estimate the endodormancy date using the parameters fitted by the
# model
for (i in 1 : length(iSeason)){
  
  num_rows <- length(iSeason[[i]])
  
  season_data <-temp_df$hourtemps[iSeason[[i]][481:num_rows],]
  
  res <- PhenoFlex(temp=season_data$Temp,
                   times=c(1: length(season_data$Temp)),
                   stopatzc=T,basic_output=F,
                   yc=104.11, 
                   zc=765, 
                   s1=7.992534e-01, 
                   Tu=2.681513e+01,
                   E0=3.240997e+03, 
                   E1=9.921809e+03,
                   A0=6.867300e+03, 
                   A1=5.939913e+13,
                   Tf=7.492625e+00, 
                   Tc=3.427934e+01, 
                   Tb=2.959986e+00, 
                   slope=3.555938e+00)
  
  season_data[,"y"]<-res$y
  season_data<-add_date(season_data)
  
  CR_full<-season_data$JDay[which(season_data$y>=104.11)[i]]
  
  # Add the endodormancy date to the primer data set
  historic_endchill[historic_endchill$Year == names(iSeason)[i], "Endodormancy"] <- CR_full
}
Historic_end <- median(historic_endchill$Endodormancy,na.rm = TRUE)
historic_endchill[,"Historic_end"]<- Historic_end
historic_endchill[,"Diff"]<- historic_endchill$Endodormancy-historic_endchill$Historic_end

# End of chill for past simulated scenarios ####
# Load the historic simulated scenarios
temps_past_scenarios <- load_temperature_scenarios("Weather/", "Tassaout_historic")

# Create a single dataset for all the simulated years
temps_past_scenarios <- bind_rows(temps_past_scenarios, .id = "Scen_year")

# Make a primer dataset to allocate the results of the endchill projection
simulated_endchill <- temps_past_scenarios %>% group_by(Scen_year, Year) %>% summarise(Endodormancy = NA)

# Define the scenario years to be used in the loop
scen_years <- unique(simulated_endchill$Scen_year)

# Implement the for loop
for (scen_year in scen_years){
  
  # Subset a temporary data frame according to scenario and climate model
  temp_df <- filter(temps_past_scenarios, Scen_year == scen_year)
  
  # Generate hourly temperatures in the temporary data frame
  temp_df <- stack_hourly_temps(temp_df, latitude = 31.82)
  
  # Define the seasons to be used for predicting the phenology
  iSeason <- genSeason(temp_df$hourtemps, mrange = c(11, 6), years = c(2002 : 2101))
  
  # Change the names of the list of seasons to be used as index in the next for loop
  names(iSeason) <- 2002 : 2101
  
  # Implement a for loop that runs over the list of seasons to estimate the endodormancy date using the parameters fitted by the
  # model
  for (i in 1 : length(iSeason)){
    
    num_rows <- length(iSeason[[i]])
    
    season_data <-temp_df$hourtemps[iSeason[[i]][481:num_rows],]
    
    res <- PhenoFlex(temp=season_data$Temp,
                          times=c(1: length(season_data$Temp)),
                          stopatzc=T,basic_output=F,
                          yc=104.11, 
                          zc=765, 
                          s1=7.992534e-01, 
                          Tu=2.681513e+01,
                          E0=3.240997e+03, 
                          E1=9.921809e+03,
                          A0=6.867300e+03, 
                          A1=5.939913e+13,
                          Tf=7.492625e+00, 
                          Tc=3.427934e+01, 
                          Tb=2.959986e+00, 
                          slope=3.555938e+00)
    
    season_data[,"y"]<-res$y
    season_data<-add_date(season_data)
    
    CR_full<-season_data$JDay[which(season_data$y>=104.11)[i]]
    
    # Add the endodormancy date to the primer data set
    simulated_endchill[simulated_endchill$Scen_year == scen_year & 
                      simulated_endchill$Year == names(iSeason)[i], "Endodormancy"] <- CR_full
  }
}
simulated_endchill[,"Historic_end"]<- Historic_end
simulated_endchill[,"Diff"]<- simulated_endchill$Endodormancy-simulated_endchill$Historic_end

# End of chill for future scenarios ####
future_temps <- list("Tassaout_2050_rcp45" = load_temperature_scenarios("Weather", "Tassaout_2050_rcp45"),
                     "Tassaout_2050_rcp85" = load_temperature_scenarios("Weather", "Tassaout_2050_rcp85"),
                     "Tassaout_2085_rcp45" = load_temperature_scenarios("Weather", "Tassaout_2085_rcp45"),
                     "Tassaout_2085_rcp85" = load_temperature_scenarios("Weather", "Tassaout_2085_rcp85"))

# Combine the datasets of each list into a single dataset per scenario
future_temps <- data.frame(
  bind_rows(
    lapply(future_temps, function (x) bind_rows(x, .id = "GCM")), .id = "List"))

# Re-structure the data. Add the RCP, Time and RCP_Time columns
future_temps <- future_temps %>%
  mutate(RCP = if_else(substr(.$List, 15, 20) == "rcp45", "RCP4.5", "RCP8.5"),
         Time = substr(.$List, 10, 13)) %>% 
  select(DATE, Year, Month, Day, Tmin, Tmax, RCP, Time, GCM) %>% 
  mutate(RCP_Time = paste0(.$RCP, "_", .$Time))

# Create a primer data frame to allocate the results for future endodormancy
future_endchill <- future_temps %>% group_by(RCP_Time, GCM, Year) %>% summarise(Endodormancy = NA)

# Define the scenarios to be used in the for loop
scenarios <- unique(future_temps$RCP_Time)

# Define the climate models to be used in the for loop
climate_models <- unique(future_temps$GCM)

# Implement the for loop
for (scenario in scenarios){
  for (climate_model in climate_models){
    
    # Subset a temporary data frame according to scenario and climate model
    temp_df <- filter(future_temps, RCP_Time == scenario & GCM == climate_model)
    
    # Generate hourly temperatures in the temporary data frame
    temp_df <- stack_hourly_temps(temp_df, latitude = 31.82)
    
    # Define the seasons to be used for predicting the phenology
    iSeason <- genSeason(temp_df$hourtemps, mrange = c(11, 6), years = c(2002 : 2101))
    
    # Change the names of the list of seasons to be used as index in the next for loop
    names(iSeason) <- 2002 : 2101
    
    for (i in 1 : length(iSeason)){
      
      num_rows <- length(iSeason[[i]])
      
      season_data <-temp_df$hourtemps[iSeason[[i]][481:num_rows],]
      
      res <- PhenoFlex(temp=season_data$Temp,
                       times=c(1: length(season_data$Temp)),
                       stopatzc=T,basic_output=F,
                       yc=104.11, 
                       zc=765,
                       s1=7.992534e-01, 
                       Tu=2.681513e+01,
                       E0=3.240997e+03, 
                       E1=9.921809e+03,
                       A0=6.867300e+03, 
                       A1=5.939913e+13,
                       Tf=7.492625e+00, 
                       Tc=3.427934e+01, 
                       Tb=2.959986e+00, 
                       slope=3.555938e+00)
      
      season_data[,"y"]<-res$y
      season_data<-add_date(season_data)
      
      CR_full<-season_data$JDay[which(season_data$y>=104.11)[i]]
      
      # Add the bloom date to the primer data set
      future_endchill[future_endchill$RCP_Time == scenario & 
                     future_endchill$GCM == climate_model & 
                     future_endchill$Year == names(iSeason)[i],"Endodormancy"] <- CR_full
    }
  }
}

colnames(future_endchill)[colnames(future_endchill) == "Year"] <- "End_year"
future_endchill[,"Historic_end"]<- Historic_end
future_endchill[,"Diff"]<- future_endchill$Endodormancy-future_endchill$Historic_end
future_endchill <- separate(future_endchill, col = RCP_Time, into = c("RCP", "Year"), sep = "_")

# Put data together and plot everything in one plot with ggplot2
high_endchill_hist <- historic_endchill
high_endchill_simulated <- simulated_endchill
high_endchill_future  <- future_endchill

High_chill_delay <- plot_highchill_delay(high_endchill_hist,high_endchill_simulated,high_endchill_future,
                 metric = "Diff",
                 axis_label = "") &
  theme(legend.position = "none")

# Plotting figure 3 ####
High_chill_delay / Mid_chill_delay / Low_chill_delay

ggsave("figures/Figure_3.png", width = 22, height = 40, units = "cm", dpi = 600)



