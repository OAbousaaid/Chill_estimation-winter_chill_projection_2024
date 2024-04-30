# In this script I will do chill projections for the respective phases of the studied cultivars


# Phase one for high-chill cultivar (Leccino) ####
# Start date
as.POSIXlt(c("21.11"), format = "%d.%m")$yday+1
# End_date 
as.POSIXlt(c("24.02"), format = "%d.%m")$yday+1

# Phase high_chill (325-55) ####
Temps<-load_temperature_scenarios("Weather","Tassaout_historic")
chill_past_scenarios_high_chill<-tempResponse_daily_list(
  Temps,
  latitude=31.82,
  Start_JDay = 325,
  End_JDay = 55,
  models=list(Chill_CP=Calib_Dynamic_Model),
  misstolerance = 10)

chill_observed_high_chill<-tempResponse_daily_list(
  tassaout_weather,
  latitude=31.82,
  Start_JDay = 325,
  End_JDay = 55,
  models=list(Chill_CP=Calib_Dynamic_Model),
  misstolerance = 10)

save_temperature_scenarios(chill_past_scenarios_high_chill,
                           "High_Chill",
                           "Tassaout_historic")
save_temperature_scenarios(chill_observed_high_chill,
                           "High_Chill",
                           "Tassaout_observed")

# We put the data in a list
chill_past_scenarios_high_chill<-load_temperature_scenarios(
  "High_Chill",
  "Tassaout_historic")

chill_observed_high_chill<-load_temperature_scenarios(
  "High_Chill",
  "Tassaout_observed")

High_chills <-make_climate_scenario(
  chill_past_scenarios_high_chill,
  caption = "Historic",
  historic_data = chill_observed_high_chill,
  time_series = TRUE)

# Visualize a first result
plot_climate_scenarios(
  climate_scenario_list=High_chills,
  metric="Chill_CP",
  metric_label="Chill (Chill Portions)")

# Future chill
for(RCP in RCPs)
  for(Time in Times)
  {
    Temps<-load_temperature_scenarios(
      "Weather",
      paste0("Tassaout_",Time,"_",RCP))
    High_chill<-tempResponse_daily_list(
      Temps,
      latitude=31.82,
      Start_JDay = 325,
      End_JDay = 55,
      models=list(Chill_CP=Calib_Dynamic_Model),
      misstolerance = 10)
    save_temperature_scenarios(
      High_chill,
      "High_Chill",
      paste0("Tassaout_",Time,"_",RCP))
  }

# Now we add them to High_chills object
for(RCP in RCPs)
  for(Time in Times)
  {
    High_chill<-load_temperature_scenarios(
      "High_Chill",
      paste0("Tassaout_",Time,"_",RCP))
    if(RCP=="rcp45") RCPcaption <- "RCP4.5"
    if(RCP=="rcp85") RCPcaption <- "RCP8.5"
    if(Time=="2050") Time_caption <- "2050"
    if(Time=="2085") Time_caption <- "2085"
    High_chills <-make_climate_scenario(
      High_chill,
      caption =c(RCPcaption, Time_caption),
      add_to = High_chills)
  }

# We can plot this the old way or using ggplot2
plot_climate_scenarios(
  climate_scenario_list=High_chills,
  metric="Chill_CP",
  metric_label="Chill (Chill Portions)",
  texcex=1.5)

# Put everything in a data frame and plot with ggplot2

for(nam in names(High_chills[[1]]$data))
{
  ch<-High_chills[[1]]$data[[nam]]
  ch[,"GCM"]<-"none"
  ch[,"RCP"]<-"none"
  ch[,"Year"]<-as.numeric(nam)
  if(nam==names(High_chills[[1]]$data)[1])
    past_simulated<-ch else
      past_simulated<-rbind(past_simulated,ch)
}

past_simulated["Scenario"] <- "Historic"

# Past observed data
past_observed <- High_chills[[1]][["historic_data"]]

# We do the same thing for the future data
for(i in 2:length(High_chills))
  for(nam in names(High_chills[[i]]$data))
  {ch<-High_chills[[i]]$data[[nam]]
  ch[,"GCM"]<-nam
  ch[,"RCP"]<-High_chills[[i]]$caption[1]
  ch[,"Year"]<-High_chills[[i]]$caption[2]
  if(i==2&nam==names(High_chills[[i]]$data)[1])
    future_data<-ch else
      future_data<-rbind(future_data,ch)
  }

# Chill plot
high_chill_projection <- plot_scenarios_ggg_highchill(past_observed=past_observed,
                  past_simulated=past_simulated,
                  future_data=future_data,
                  metric="Chill_CP",
                  axis_label="")&
  theme(legend.position = "none")

# Phase one for mid-chill cultivar (PM,...) ####
# Start date
as.POSIXlt(c("26.12"), format = "%d.%m")$yday
# End_date 
as.POSIXlt(c("21.02"), format = "%d.%m")$yday

# Phase mid_chill (360-52) ####

Temps<-load_temperature_scenarios("Weather","Tassaout_historic")
mid_chill_past_scenarios<-tempResponse_daily_list(
  Temps,
  latitude=31.82,
  Start_JDay = 360,
  End_JDay = 51,
  models=list(Chill_CP=Calib_Dynamic_Model),
  misstolerance = 10)

mid_chill_observed<-tempResponse_daily_list(
  tassaout_weather,
  latitude=31.82,
  Start_JDay = 360,
  End_JDay = 51,
  models=list(Chill_CP=Calib_Dynamic_Model),
  misstolerance = 10)

save_temperature_scenarios(mid_chill_past_scenarios,
                           "Mid_Chill",
                           "Tassaout_historic")
save_temperature_scenarios(mid_chill_observed,
                           "Mid_Chill",
                           "Tassaout_observed")

# We then plot historic and observed chill with the function make_climate_scenario
mid_chill_past_scenarios<-load_temperature_scenarios(
  "Mid_Chill",
  "Tassaout_historic")

mid_chill_observed<-load_temperature_scenarios(
  "Mid_Chill",
  "Tassaout_observed")

Mid_chills <-make_climate_scenario(
  mid_chill_past_scenarios,
  caption = "Historic",
  historic_data = mid_chill_observed,
  time_series = TRUE)

# Visualize a first result
plot_climate_scenarios(
  climate_scenario_list=Mid_chills,
  metric="Chill_CP",
  metric_label="Chill (Chill Portions)")

# Future chill
for(RCP in RCPs)
  for(Time in Times)
  {
    Temps<-load_temperature_scenarios(
      "Weather",
      paste0("Tassaout_",Time,"_",RCP))
    Mid_chill<-tempResponse_daily_list(
      Temps,
      latitude=31.82,
      Start_JDay = 360,
      End_JDay = 51,
      models=list(Chill_CP=Calib_Dynamic_Model),
      misstolerance = 10)
    save_temperature_scenarios(
      Mid_chill,
      "Mid_Chill",
      paste0("Tassaout_",Time,"_",RCP))
  }

# Now we add them to Mid_chills
for(RCP in RCPs)
  for(Time in Times)
  {
    Mid_chill<-load_temperature_scenarios(
      "Mid_Chill",
      paste0("Tassaout_",Time,"_",RCP))
    if(RCP=="rcp45") RCPcaption <- "RCP4.5"
    if(RCP=="rcp85") RCPcaption <- "RCP8.5"
    if(Time=="2050") Time_caption <- "2050"
    if(Time=="2085") Time_caption <- "2085"
    Mid_chills <-make_climate_scenario(
      Mid_chill,
      caption =c(RCPcaption, Time_caption),
      add_to = Mid_chills)
  }

# Now we can plot everything the old way
plot_climate_scenarios(
  climate_scenario_list=Mid_chills,
  metric="Chill_CP",
  metric_label="Chill (Chill Portions)",
  texcex=1.5)

# Put everything in a data frame and plot with ggplot2

for(nam in names(Mid_chills[[1]]$data))
{
  ch<-Mid_chills[[1]]$data[[nam]]
  ch[,"GCM"]<-"none"
  ch[,"RCP"]<-"none"
  ch[,"Year"]<-as.numeric(nam)
  if(nam==names(Mid_chills[[1]]$data)[1])
    past_simulated<-ch else
      past_simulated<-rbind(past_simulated,ch)
}

past_simulated["Scenario"] <- "Historic"

# Past observed data
past_observed <- Mid_chills[[1]][["historic_data"]]

# We do the same thing for the future data
for(i in 2:length(Mid_chills))
  for(nam in names(Mid_chills[[i]]$data))
  {ch<-Mid_chills[[i]]$data[[nam]]
  ch[,"GCM"]<-nam
  ch[,"RCP"]<-Mid_chills[[i]]$caption[1]
  ch[,"Year"]<-Mid_chills[[i]]$caption[2]
  if(i==2&nam==names(Mid_chills[[i]]$data)[1])
    future_data<-ch else
      future_data<-rbind(future_data,ch)
  }

# Chill plot
mid_chill_projection <- plot_scenarios_ggg_midchill(past_observed=past_observed,
                  past_simulated=past_simulated,
                  future_data=future_data,
                  metric="Chill_CP",
                  axis_label="Chill (in Chill Portions)")&
  theme(legend.position = "none")

# Phase one for low-chill cultivar (Sabatera,...) ####
# Start date
as.POSIXlt(c("01.01"), format = "%d.%m")$yday+1
# End_date 
as.POSIXlt(c("25.01"), format = "%d.%m")$yday+1

# Phase low_chill (01-25) ####

Temps<-load_temperature_scenarios("Weather","Tassaout_historic")
low_chill_past_scenarios<-tempResponse_daily_list(
  Temps,
  latitude=31.82,
  Start_JDay = 1,
  End_JDay = 25,
  models=list(Chill_CP=Calib_Dynamic_Model),
  misstolerance = 10)

low_chill_observed<-tempResponse_daily_list(
  tassaout_weather,
  latitude=31.82,
  Start_JDay = 1,
  End_JDay = 25,
  models=list(Chill_CP=Calib_Dynamic_Model),
  misstolerance = 10)

save_temperature_scenarios(low_chill_past_scenarios,
                           "Low_Chill",
                           "Tassaout_historic")
save_temperature_scenarios(low_chill_observed,
                           "Low_Chill",
                           "Tassaout_observed")

# We then plot historic and observed chill with the function make_climate_scenario
low_chill_past_scenarios<-load_temperature_scenarios(
  "Low_Chill",
  "Tassaout_historic")

low_chill_observed<-load_temperature_scenarios(
  "Low_Chill",
  "Tassaout_observed")

Low_chills <-make_climate_scenario(
  low_chill_past_scenarios,
  caption = "Historic",
  historic_data = low_chill_observed,
  time_series = TRUE)

# Visualize first plot
plot_climate_scenarios(
  climate_scenario_list=Low_chills,
  metric="Chill_CP",
  metric_label="Chill (Chill Portions)")

# Future chill
for(RCP in RCPs)
  for(Time in Times)
  {
    Temps<-load_temperature_scenarios(
      "Weather",
      paste0("Tassaout_",Time,"_",RCP))
    Low_chill<-tempResponse_daily_list(
      Temps,
      latitude=31.82,
      Start_JDay = 1,
      End_JDay = 25,
      models=list(Chill_CP=Calib_Dynamic_Model),
      misstolerance = 10)
    save_temperature_scenarios(
      Low_chill,
      "Low_Chill",
      paste0("Tassaout_",Time,"_",RCP))
  }

# Now we add them to low_chills object
for(RCP in RCPs)
  for(Time in Times)
  {
    Low_chill<-load_temperature_scenarios(
      "Low_Chill",
      paste0("Tassaout_",Time,"_",RCP))
    if(RCP=="rcp45") RCPcaption <- "RCP4.5"
    if(RCP=="rcp85") RCPcaption <- "RCP8.5"
    if(Time=="2050") Time_caption <- "2050"
    if(Time=="2085") Time_caption <- "2085"
    Low_chills <-make_climate_scenario(
      Low_chill,
      caption =c(RCPcaption, Time_caption),
      add_to = Low_chills)
  }

# Now we can plot everything
plot_climate_scenarios(
  climate_scenario_list=Low_chills,
  metric="Chill_CP",
  metric_label="Chill (Chill Portions)",
  texcex=1.5)

# Put everything in a data frame and plot with ggplot2

for(nam in names(Low_chills[[1]]$data))
{
  ch<-Low_chills[[1]]$data[[nam]]
  ch[,"GCM"]<-"none"
  ch[,"RCP"]<-"none"
  ch[,"Year"]<-as.numeric(nam)
  if(nam==names(Low_chills[[1]]$data)[1])
    past_simulated<-ch else
      past_simulated<-rbind(past_simulated,ch)
}

past_simulated["Scenario"] <- "Historic"

# Past observed data
past_observed <- Low_chills[[1]][["historic_data"]]

# We do the same thing for the future data
for(i in 2:length(Low_chills))
  for(nam in names(Low_chills[[i]]$data))
  {ch<-Low_chills[[i]]$data[[nam]]
  ch[,"GCM"]<-nam
  ch[,"RCP"]<-Low_chills[[i]]$caption[1]
  ch[,"Year"]<-Low_chills[[i]]$caption[2]
  if(i==2&nam==names(Low_chills[[i]]$data)[1])
    future_data<-ch else
      future_data<-rbind(future_data,ch)
  }

# Chill plot
low_chill_projection <- plot_scenarios_ggg_lowchill(past_observed=past_observed,
                  past_simulated=past_simulated,
                  future_data=future_data,
                  metric="Chill_CP",
                  axis_label="")

# Plotting figure 2 ####
library(patchwork)
high_chill_projection / mid_chill_projection / low_chill_projection

ggsave("figures/Figure_2.png", width = 22, height = 40, units = "cm", dpi = 600)
