# In this script I will compare the dynamic model default and calibrated

# Start date
as.POSIXlt(c("01.11"), format = "%d.%m")$yday
# End date
as.POSIXlt(c("01.03"), format = "%d.%m")$yday

# Compute chill ####
Temps<-load_temperature_scenarios("Weather","Tassaout_historic")
chill_past_scenarios<-tempResponse_daily_list(
  Temps,
  latitude=31.82,
  Start_JDay = 305,
  End_JDay = 60,
  models=list(Chill_CP=Dynamic_Model,Calib_Chill_CP=Calib_Dynamic_Model),
  misstolerance = 10)

chill_observed<-tempResponse_daily_list(
  tassaout_weather,
  latitude=31.82,
  Start_JDay = 305,
  End_JDay = 60,
  models=list(Chill_CP=Dynamic_Model,Calib_Chill_CP=Calib_Dynamic_Model),
  misstolerance = 10)

save_temperature_scenarios(chill_past_scenarios,
                           "Chill",
                           "Tassaout_historic")
save_temperature_scenarios(chill_observed,
                           "Chill",
                           "Tassaout_observed")

# We then plot historic and observed chill with the function make_climate_scenario

chill_past_scenarios<-load_temperature_scenarios(
  "Chill",
  "Tassaout_historic")
chill_observed<-load_temperature_scenarios(
  "Chill",
  "Tassaout_observed")

chills <-make_climate_scenario(
  chill_past_scenarios,
  caption = "Historic",
  historic_data = chill_observed,
  time_series = TRUE)

# Now we compute chill for future scenarios
# We download all temperatures scenarios and we calculate chill
for(RCP in RCPs)
  for(Time in Times)
  {
    Temps<-load_temperature_scenarios(
      "Weather",
      paste0("Tassaout_",Time,"_",RCP))
    chill<-tempResponse_daily_list(
      Temps,
      latitude=31.82,
      Start_JDay = 305,
      End_JDay = 60,
      models=list(Chill_CP=Dynamic_Model,Calib_Chill_CP=Calib_Dynamic_Model),
      misstolerance = 10)
    save_temperature_scenarios(
      chill,
      "Chill",
      paste0("Tassaout_",Time,"_",RCP))
  }

# Now we add them to chill object to have everything in the same list
for(RCP in RCPs)
  for(Time in Times)
  {
    chill<-load_temperature_scenarios(
      "Chill",
      paste0("Tassaout_",Time,"_",RCP))
    if(RCP=="rcp45") RCPcaption <- "RCP4.5"
    if(RCP=="rcp85") RCPcaption <- "RCP8.5"
    if(Time=="2050") Time_caption <- "2050"
    if(Time=="2085") Time_caption <- "2085"
    chills <-make_climate_scenario(
      chill,
      caption =c(RCPcaption, Time_caption),
      add_to = chills)
  }


# We will try here to generate some appealing plots with ggplot
# We'll first process the past scenarios (element 1 of the chills list).
# Within the data element, we have a list of multiple data.frames for
# the various past scenarios.
# Using a 'for' loop, we cycle through all these data.frames.

for(nam in names(chills[[1]]$data))
{
  ch<-chills[[1]]$data[[nam]]
  ch[,"GCM"]<-"none"
  ch[,"RCP"]<-"none"
  ch[,"Year"]<-as.numeric(nam)
  if(nam==names(chills[[1]]$data)[1])
    past_simulated<-ch else
      past_simulated<-rbind(past_simulated,ch)
}

past_simulated["Scenario"] <- "Historic"

# Example of data frame
kable(past_simulated[1:5,])  %>%
  kable_styling("striped", position = "left",font_size = 8)

# Past observed data
past_observed <- chills[[1]][["historic_data"]]

# We do the same thing for the future data
for(i in 2:length(chills))
  for(nam in names(chills[[i]]$data))
  {ch<-chills[[i]]$data[[nam]]
  ch[,"GCM"]<-nam
  ch[,"RCP"]<-chills[[i]]$caption[1]
  ch[,"Year"]<-chills[[i]]$caption[2]
  if(i==2&nam==names(chills[[i]]$data)[1])
    future_data<-ch else
      future_data<-rbind(future_data,ch)
  }

# Example of data frame
kable(future_data[1:5,])  %>%
  kable_styling("striped", position = "left",font_size = 8)

# Using the function written by Luedeling to produce the plot with ggplot
plot_scenarios_gg<-function(past_observed,
                            past_simulated,
                            future_data,
                            metric,
                            axis_label)
{
  rng<-range(past_observed[[metric]],
             past_simulated[[metric]],
             future_data[[metric]])  
  past_plot<-ggplot() +
    geom_boxplot(data = past_simulated,
                 aes_string("as.numeric(Year)",metric,group="Year"),
                 fill="skyblue") +
    geom_hline(aes(yintercept = median(past_observed$Chill_CP)), color = "blue2", linetype = "solid") +
    scale_y_continuous(limits = c(0, round(round(1.1*rng[2])))) +
    labs(x = "Year", y = axis_label) +
    facet_grid(~ Scenario) +
    theme_bw(base_size = 15) +  
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          axis.text.x = element_text(angle=45, hjust=1)) +
    geom_point(data = past_observed,
               aes_string("End_year",metric),
               col="blue")
  
  future_plot_list<-list()
  
  for(y in c(2050,2085))
  {
    future_plot_list[[which(y == c(2050,2085))]] <-
      ggplot(data= future_data[which(future_data$Year==y),]) +
      geom_boxplot(aes_string("GCM", metric, fill="GCM")) +
      geom_hline(aes(yintercept = median(past_observed$Chill_CP)), color = "blue2", linetype = "solid") +
      facet_wrap(vars(RCP)) +
      scale_x_discrete(labels = NULL, expand = expansion(add = 1)) +
      scale_y_continuous(limits = c(0, round(round(1.1*rng[2])))) +
      geom_text_npc(aes(npcx = "center", npcy = "top", label = Year),
                    size = 5) +
      theme_bw(base_size = 15) +
      theme(axis.ticks.y = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom",
            legend.margin = margin(0, 0, 0, 0, "cm"),
            legend.background = element_rect(),
            strip.background = element_blank(),
            strip.text = element_text(face = "bold"),
            legend.box.spacing = unit(0, "cm"),
            plot.subtitle = element_text(hjust = 0.5,
                                         vjust = -1,
                                         size = 15 * 1.05,
                                         face = "bold")) 
  }
  
  plot<- (past_plot +
            future_plot_list +
            plot_layout(guides = "collect",
                        widths = c(1,rep(1.8,length(future_plot_list))))
  ) & theme(legend.position = "bottom",
            legend.text = element_text(size=8),
            legend.title = element_text(size=10),
            axis.title.x=element_blank())
  plot
  
}

# Chill plot
plot_scenarios_gg(past_observed=past_observed,
                  past_simulated=past_simulated,
                  future_data=future_data,
                  metric="Chill_CP",
                  axis_label="Chill (in Chill Portions)")

# The three data base will be merged in one with scenario as first column and chill metrics in the sceond
# and third columns

View(future_data)
future_data[,"Scenario"] <- paste(future_data$RCP,future_data$Year,sep = '_')
past_observed[,"Scenario"] <- "Past_observed"
past_simulated$Scenario <- "Past_simulated"

future_past_data <- rbind(future_data[,c(11,6,7)],past_simulated[,c(11,6,7)],past_observed[,c(8,6,7)])

View(future_past_data)
future_past_data$Scenario <- as.factor(future_past_data$Scenario)
levels(future_past_data$Scenario)

legend_names <- c("Past_observed"="Past observed",
                  "Past_simulated"="Past simulated",
                  "RCP4.5_2050"="RCP4.5 2050",
                  "RCP4.5_2085"="RCP4.5 2085",
                  "RCP8.5_2050"="RCP8.5 2050",
                  "RCP8.5_2085"="RCP8.5 2080")

future_past_data$Scenario <- factor(future_past_data$Scenario, levels = names(legend_names), labels = legend_names)

# Ploting data ####

chill_plot <- ggplot(future_past_data,aes(Scenario,Chill_CP,fill=Scenario))+
  geom_boxplot()+
  theme_bw(base_size = 13)+
  labs(y = "Chill Portions", x = "") +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title = element_text(face = "bold"))

calib_chill_plot <- ggplot(future_past_data,aes(Scenario,Calib_Chill_CP,fill=Scenario))+
  geom_boxplot()+
  theme_bw(base_size = 13)+
  labs(y = "Chill Portions", x = "") +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title = element_text(face = "bold"))

# Temperature response for each model ####
# Upload functions from helper functions by Fernandez et al., 2022
source("0_helper_functions.R")

# Default par for the dynamic model
#                                yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
Def_Pheno_fit <- Pheno_fit
Def_Pheno_fit$par <- c(5.800000e+01, 7.850000e+02, 7.992534e-01, 2.681513e+01,       4153.5,      12888.8,       139500,    2.567e+18,          3.85, 3.427934e+01, 2.959986e+00,         1.6)

# Data frame for temperature response
temp_response <- data.frame(Temp = seq(-5, 60, 0.1),
                            Chill_res = gen_bell(Pheno_fit$par, seq(-5, 60, 0.1)),
                            Chill_res_def = gen_bell(Def_Pheno_fit$par, seq(-5, 60, 0.1)))

# Pivot longer to get suitable format for ggplot
temp_response <- pivot_longer(temp_response, -Temp, names_to = "Var", values_to = "Response")

# Chill plot
chill_response_plot <- ggplot(filter(temp_response, Var == "Chill_res"), aes(Temp, Response)) +
  geom_line(size = 1, color = "blue4") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_x_continuous(limits = c(-5, 25),
                     labels = function (x) paste0(x, " °C")) +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text = element_text(size = 8),
        strip.text.y = element_blank(),
        strip.background = element_blank())

def_chill_response_plot <- ggplot(filter(temp_response, Var == "Chill_res_def"), aes(Temp, Response)) +
  geom_line(size = 1, color = "blue4") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_x_continuous(limits = c(-5, 25),
                     labels = function (x) paste0(x, " °C")) +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text = element_text(size = 8),
        strip.text.y = element_blank(),
        strip.background = element_blank())

library(patchwork)

Figure_1<-(chill_plot | (def_chill_response_plot+ggtitle('Temperature response'))) /
  (calib_chill_plot | chill_response_plot) +  plot_layout(guides = 'collect') &
  theme(legend.position='bottom')
  
ggsave("figures/Figure_1.png", width = 22, height = 16, units = "cm", dpi = 600)
