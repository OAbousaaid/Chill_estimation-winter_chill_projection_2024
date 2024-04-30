# In this Script I will generate temperatures for the past and the future for Tassaout

# Upload libraries
library(chillR)
library(readxl)
library(ggplot2)
library(reshape2)
library(kableExtra)
library(ggpmisc)
library(patchwork)

# Upload temperature data (From Tassaout weather station)
tassaout_weather <- read_xlsx("Data/Temperature_72_22_max_min.xlsx")
tassaout_weather <- as.data.frame(tassaout_weather)

# Making past scenarios
# 1st a scenario for the median
years <- c(1972:2022)
median(years)

scenario_1997<-temperature_scenario_from_records(weather=tassaout_weather,year=1997)

# 2nd for the years we are interested in
all_past_scenarios<-temperature_scenario_from_records(
  weather=tassaout_weather,
  year=c(1975,1980,1985,1990,1995,2000,2005,2010,2015,2020))

adjusted_scenarios<-temperature_scenario_baseline_adjustment(
  baseline=scenario_1997,
  temperature_scenario = all_past_scenarios)

all_past_scenario_temps<-temperature_generation(
  weather=tassaout_weather,
  years=c(1972,2022),
  sim_years=c(2001,2100),
  temperature_scenario = adjusted_scenarios)

save_temperature_scenarios(
  all_past_scenario_temps,
  "Weather",
  "Tassaout_historic")

# Now we have temperatures for all past scenarios.

# Now it's time to make future scenarios
# From the ClimateWizard accessed with ChillR provides data for the period 1950-2005 and requires
# a baseline of 20 years
# We are going to choose the period between 1975 and 2005 to have a median year of 1990 (intger) 
# 31 years as a baseline

# The ClimateWizard database provides only two RCPs 
RCPs<-c("rcp45","rcp85")
Times<-c(2050,2085)

for(RCP in RCPs)
  for(Time in Times)
  {start_year <- Time-15
  end_year <- Time+15
  clim_scen <-getClimateWizardData(
    c(longitude = -7.432778,latitude = 31.819444),
    RCP,
    start_year,
    end_year,
    temperature_generation_scenarios = TRUE,
    baseline =c(1975, 2005),
    metric = "monthly_min_max_temps",
    GCMs = "all")
  save_temperature_scenarios(clim_scen,
                             "ClimateWizard",
                             paste0("Tassaout_futures_",Time,"_",RCP))}


# Now that we generated scenarios for the period between 1975-2005 we want to extend it to our period
# of interest 1972-2022 the median for this period is 1997 which is different from 1990. 
# So we need a baseline adjustment
scenario_1990<-temperature_scenario_from_records(tassaout_weather,1990)
scenario_1997<-temperature_scenario_from_records(tassaout_weather,1997)
adjustment_scenario<-temperature_scenario_baseline_adjustment(scenario_1997,scenario_1990)

adjustment_scenario

# Now we can generate future temperatures
for(RCP in RCPs)
  for(Time in Times)
  {
    clim_scen<-load_ClimateWizard_scenarios(
      "ClimateWizard",
      paste0("Tassaout_futures_",Time,"_",RCP))
    clim_scen_adjusted<-
      temperature_scenario_baseline_adjustment(
        baseline_temperature_scenario=adjustment_scenario,
        temperature_scenario=clim_scen)
    Temps<-temperature_generation(
      weather=tassaout_weather, 
      years=c(1972,2022),
      sim_years=c(2001,2101),
      temperature_scenario = clim_scen_adjusted)
    
    save_temperature_scenarios(
      Temps,
      "Weather",
      paste0("Tassaout_",Time,"_",RCP))
  }
