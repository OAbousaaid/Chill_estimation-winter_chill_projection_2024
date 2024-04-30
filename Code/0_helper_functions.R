genSeason <- function(temps, mrange = c(8, 6), years) {
  stopifnot(length(mrange) == 2)
  N <- length(years)
  stopifnot(N >= 1)
  Season <- list()
  if(inherits(temps, "list"))  temperatures<-temps[[1]] else temperatures<-temps
  for(j in c(1 : N)) {
    ## season starts in November of the preceding year
    ## the winter season from August to June
    ## we add the Year as the last element to ship it to sapply
    Season[[j]] <- c(which(((temperatures$Month %in% c(mrange[1] : 12)) &
                              (temperatures$Year == years[j] - 1))),
                     which(((temperatures$Month %in% c(1 : mrange[2])) &
                              (temperatures$Year == years[j]))))
  }
  return(invisible(Season))
}


# Helper to wrap the DynModel_driver function from chillR

apply_const_temp <- function(temp, A0, A1, E0, E1, Tf, slope, portions = 2400, deg_celsius = TRUE)
  {
    temp_vector <- rep(temp, times = portions)
    res <- chillR::DynModel_driver(temp = temp_vector,
                                   A0 = A0,
                                   A1 = A1,
                                   E0 = E0,
                                   E1 = E1,
                                   Tf = Tf,
                                   slope = slope,
                                   deg_celsius = deg_celsius)
    return(invisible(res$y[length(res$y)]))
}

# Helper to generate a bell shape based on chill response

gen_bell <- function(par, temp_values = seq(-5, 20, 0.1)) {
  E0 <- par[5]
  E1 <- par[6]
  A0 <- par[7]
  A1 <- par[8]
  Tf <- par[9]
  slope <- par[12]
  
  y <- c()
  for(i in seq_along(temp_values)) {
    y[i] <- apply_const_temp(temp = temp_values[i],
                             A0 = A0,
                             A1 = A1,
                             E0 = E0,
                             E1 = E1,
                             Tf = Tf,
                             slope = slope)
  }
  return(invisible(y))
}


# Helper to generate the response to GDH accumulation

GDH_response <- function(par, T){
  
  Tb <- par[11]
  Tu <- par[4]
  Tc <- par[10]
  
  GDH_weight <- rep(0, length(T))
  
  GDH_weight[which(T >= Tb & T <= Tu)] <- 1/2 * (1 + cos(pi + pi * (T[which(T >= Tb & T <= Tu)] - Tb)/(Tu - Tb)))

  GDH_weight[which(T > Tu & T <= Tc)] <- (1 + cos(pi/2 + pi/2 * (T[which(T >  Tu & T <= Tc)] -Tu)/(Tc - Tu)))
  
  return(GDH_weight)
}

# Plotting function
plot_scenarios_ggg <- function (past_observed, past_simulated, future_data, metric, 
          axis_label) 
{
  rng <- range(past_observed[[metric]], past_simulated[[metric]], 
               future_data[[metric]])
  past_plot <- ggplot() + geom_boxplot(data = past_simulated, aes_string("as.numeric(Year)", metric, group = "Year"), fill = "skyblue") + 
    geom_hline(aes(yintercept = median(past_observed$Chill_CP)), color = "blue1", linetype = "solid") + 
    geom_hline(aes(yintercept = median(past_observed$Chill_CP) + sd(past_observed$Chill_CP)), color = "blue2", linetype = "dashed") + 
    geom_hline(aes(yintercept = median(past_observed$Chill_CP) - sd(past_observed$Chill_CP)), color = "blue2", linetype = "dashed") + 
    scale_y_continuous(limits = c(12, round(round(1.1 * rng[2])))) + 
    labs(x = "Year", y = axis_label) + facet_grid(~Scenario) + 
    theme_bw(base_size = 15) + theme(strip.background = element_blank(), strip.text = element_text(face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1)) + 
    geom_point(data = past_observed, aes_string("End_year", metric), col = "blue")
  
  future_plot_list <- list()
  for (y in c(2050, 2085)) {
    future_plot_list[[which(y == c(2050, 2085))]] <- ggplot(data = future_data[which(future_data$Year == y), ]) + geom_boxplot(aes_string("GCM", metric, fill = "GCM")) + 
      geom_hline(aes(yintercept = median(past_observed$Chill_CP)), color = "blue1", linetype = "solid") + 
      geom_hline(aes(yintercept = median(past_observed$Chill_CP) + sd(past_observed$Chill_CP)), color = "blue2", linetype = "dashed") +
      geom_hline(aes(yintercept = median(past_observed$Chill_CP) - sd(past_observed$Chill_CP)), color = "blue2", linetype = "dashed") + facet_wrap(vars(RCP)) + 
      scale_x_discrete(labels = NULL, expand = expansion(add = 1)) + 
      scale_y_continuous(limits = c(12, round(round(1.1 * rng[2])))) + geom_text_npc(aes(npcx = "center", npcy = "top", label = Year), size = 5) + 
      theme_bw(base_size = 15) + 
      theme(axis.ticks.y = element_blank(), axis.text = element_blank(), 
            axis.title = element_blank(), legend.position = "bottom", 
            legend.margin = margin(0, 0, 0, 0, "cm"), legend.background = element_rect(), 
            strip.background = element_blank(), strip.text = element_text(face = "bold"), 
            legend.box.spacing = unit(0, "cm"), plot.subtitle = element_text(hjust = 0.5, 
                                                                             vjust = -1, size = 15 * 1.05, face = "bold"))
  }
  plot <- (past_plot + future_plot_list + plot_layout(guides = "collect", 
                                                      widths = c(1, rep(1.8, length(future_plot_list))))) & 
    theme(legend.position = "bottom", legend.text = element_text(size = 8), 
          legend.title = element_text(size = 10), axis.title.x = element_blank())
  plot
}


plot_scenarios_ggg_midchill <- function(past_observed, past_simulated, future_data, metric, axis_label) {
  rng <- range(past_observed[[metric]], past_simulated[[metric]], future_data[[metric]])
  past_plot <- ggplot() + geom_boxplot(data = past_simulated, aes_string("as.numeric(Year)", metric, group = "Year"), fill = "skyblue") + 
    geom_hline(aes(yintercept = median(past_observed$Chill_CP)), color = "blue1", linetype = "solid") + 
    geom_hline(aes(yintercept = median(past_observed$Chill_CP) + sd(past_observed$Chill_CP)), color = "blue2", linetype = "dashed") + 
    geom_hline(aes(yintercept = median(past_observed$Chill_CP) - sd(past_observed$Chill_CP)), color = "blue2", linetype = "dashed") + 
    scale_y_continuous(limits = c(40, round(round(1.1 * rng[2])))) + 
    labs(x = NULL, y = axis_label) + 
    facet_grid(~Scenario, labeller = labeller(Scenario = "")) +  # Remove facet titles
    theme_bw(base_size = 15) + 
    theme(strip.background = element_blank(), strip.text = element_blank(),  # Remove facet labels
          axis.text.x = element_blank(), axis.title.x = element_blank()) +  # Remove x-axis labels and title
    geom_point(data = past_observed, aes_string("End_year", metric), col = "blue")

  future_plot_list <- list()
  for (y in c(2050, 2085)) {
    future_plot_list[[which(y == c(2050, 2085))]] <- ggplot(data = future_data[which(future_data$Year == y), ]) + geom_boxplot(aes_string("GCM", metric, fill = "GCM")) + 
      geom_hline(aes(yintercept = median(past_observed$Chill_CP)), color = "blue1", linetype = "solid") + 
      geom_hline(aes(yintercept = median(past_observed$Chill_CP) + sd(past_observed$Chill_CP)), color = "blue2", linetype = "dashed") +
      geom_hline(aes(yintercept = median(past_observed$Chill_CP) - sd(past_observed$Chill_CP)), color = "blue2", linetype = "dashed") + facet_wrap(vars(RCP)) + 
      scale_x_discrete(labels = NULL, expand = expansion(add = 1)) + 
      scale_y_continuous(limits = c(40, round(round(1.1 * rng[2])))) + geom_text_npc(aes(npcx = "center", npcy = "top", label = Year), size = 5) + 
      theme_bw(base_size = 15) + 
      theme(axis.ticks.y = element_blank(), axis.text = element_blank(), 
            axis.title = element_blank(), legend.position = "bottom", 
            legend.margin = margin(0, 0, 0, 0, "cm"), legend.background = element_rect(), 
            strip.background = element_blank(), strip.text = element_blank(),  # Remove facet labels
            legend.box.spacing = unit(0, "cm"), plot.subtitle = element_text(hjust = 0.5, 
                                                                             vjust = -1, size = 15 * 1.05, face = "bold"))
  }
  plot <- (past_plot + future_plot_list + plot_layout(guides = "collect", 
                                                      widths = c(1, rep(1.8, length(future_plot_list))))) & 
    theme(legend.position = "bottom", legend.text = element_text(size = 8), 
          legend.title = element_text(size = 10), axis.title.x = element_blank())
  plot
}

plot_scenarios_ggg_highchill <- function(past_observed, past_simulated, future_data, metric, axis_label) {
  rng <- range(past_observed[[metric]], past_simulated[[metric]], future_data[[metric]])
  past_plot <- ggplot() + geom_boxplot(data = past_simulated, aes_string("as.numeric(Year)", metric, group = "Year"), fill = "skyblue") + 
    geom_hline(aes(yintercept = median(past_observed$Chill_CP)), color = "blue1", linetype = "solid") + 
    geom_hline(aes(yintercept = median(past_observed$Chill_CP) + sd(past_observed$Chill_CP)), color = "blue2", linetype = "dashed") + 
    geom_hline(aes(yintercept = median(past_observed$Chill_CP) - sd(past_observed$Chill_CP)), color = "blue2", linetype = "dashed") + 
    scale_y_continuous(limits = c(70, round(round(1.1 * rng[2]))), expand = c(0, 0)) +  # Set y-axis limits and remove expansion
    labs(x = NULL, y = axis_label) + 
    facet_grid(~Scenario) +  
    theme_bw(base_size = 15) + 
    theme(strip.background = element_blank(), strip.text = element_text(face = "bold"),  
          axis.text.x = element_blank(), axis.title.x = element_blank()) +  
    geom_point(data = past_observed, aes_string("End_year", metric), col = "blue")

  future_plot_list <- list()
  for (y in c(2050, 2085)) {
    future_plot_list[[which(y == c(2050, 2085))]] <- ggplot(data = future_data[which(future_data$Year == y), ]) + geom_boxplot(aes_string("GCM", metric, fill = "GCM")) + 
      geom_hline(aes(yintercept = median(past_observed$Chill_CP)), color = "blue1", linetype = "solid") + 
      geom_hline(aes(yintercept = median(past_observed$Chill_CP) + sd(past_observed$Chill_CP)), color = "blue2", linetype = "dashed") +
      geom_hline(aes(yintercept = median(past_observed$Chill_CP) - sd(past_observed$Chill_CP)), color = "blue2", linetype = "dashed") + facet_wrap(vars(RCP)) + 
      scale_x_discrete(labels = NULL, expand = expansion(add = 1)) + 
      scale_y_continuous(limits = c(70, round(round(1.1 * rng[2]))), expand = c(0, 0)) +  # Set y-axis limits and remove expansion
      geom_text_npc(aes(npcx = "center", npcy = "top", label = Year), size = 5) + 
      theme_bw(base_size = 15) + 
      theme(axis.ticks.y = element_blank(), axis.text = element_blank(), 
            axis.title = element_blank(), legend.position = "bottom", 
            legend.margin = margin(0, 0, 0, 0, "cm"), legend.background = element_rect(), 
            strip.background = element_blank(), strip.text = element_text(face = "bold"), 
            legend.box.spacing = unit(0, "cm"), plot.subtitle = element_text(hjust = 0.5, 
                                                                             vjust = -1, size = 15 * 1.05, face = "bold"))
  }
  plot <- (past_plot + future_plot_list + plot_layout(guides = "collect", 
                                                      widths = c(1, rep(1.8, length(future_plot_list))))) & 
    theme(legend.position = "bottom", legend.text = element_text(size = 8), 
          legend.title = element_text(size = 10), axis.title.x = element_blank())
  plot
}


library(ggplot2)

plot_scenarios_ggg_lowchill <- function(past_observed, past_simulated, future_data, metric, axis_label) {
  rng <- range(past_observed[[metric]], past_simulated[[metric]], future_data[[metric]])
  past_plot <- ggplot() + geom_boxplot(data = past_simulated, aes_string("as.numeric(Year)", metric, group = "Year"), fill = "skyblue") + 
    geom_hline(aes(yintercept = median(past_observed$Chill_CP)), color = "blue1", linetype = "solid") + 
    geom_hline(aes(yintercept = median(past_observed$Chill_CP) + sd(past_observed$Chill_CP)), color = "blue2", linetype = "dashed") + 
    geom_hline(aes(yintercept = median(past_observed$Chill_CP) - sd(past_observed$Chill_CP)), color = "blue2", linetype = "dashed") + 
    scale_y_continuous(limits = c(15, round(round(1.1 * rng[2])))) + 
    labs(x = "Year", y = axis_label) + facet_grid(~Scenario, labeller = labeller(Scenario = "")) +  # Remove facet titles and labels
    theme_bw(base_size = 15) + 
    theme(strip.background = element_blank(), strip.text = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +  # Remove facet titles and labels
    geom_point(data = past_observed, aes_string("End_year", metric), col = "blue")
  
  future_plot_list <- list()
  for (y in c(2050, 2085)) {
    future_plot_list[[which(y == c(2050, 2085))]] <- ggplot(data = future_data[which(future_data$Year == y), ]) + geom_boxplot(aes_string("GCM", metric, fill = "GCM")) + 
      geom_hline(aes(yintercept = median(past_observed$Chill_CP)), color = "blue1", linetype = "solid") + 
      geom_hline(aes(yintercept = median(past_observed$Chill_CP) + sd(past_observed$Chill_CP)), color = "blue2", linetype = "dashed") +
      geom_hline(aes(yintercept = median(past_observed$Chill_CP) - sd(past_observed$Chill_CP)), color = "blue2", linetype = "dashed") + facet_wrap(vars(RCP)) + 
      scale_x_discrete(labels = NULL, expand = expansion(add = 1)) + 
      scale_y_continuous(limits = c(15, round(round(1.1 * rng[2])))) + geom_text_npc(aes(npcx = "center", npcy = "top", label = Year), size = 5) + 
      theme_bw(base_size = 15) + 
      theme(axis.ticks.y = element_blank(), axis.text = element_blank(), 
            axis.title = element_blank(), legend.position = "bottom", 
            legend.margin = margin(0, 0, 0, 0, "cm"), legend.background = element_rect(), 
            strip.background = element_blank(), strip.text = element_blank(),  # Remove facet titles and labels
            legend.box.spacing = unit(0, "cm"), plot.subtitle = element_text(hjust = 0.5, 
                                                                             vjust = -1, size = 15 * 1.05, face = "bold"))
  }
  plot <- (past_plot + future_plot_list + plot_layout(guides = "collect", 
                                                      widths = c(1, rep(1.8, length(future_plot_list))))) & 
    theme(legend.position = "bottom", legend.text = element_text(size = 8), 
          legend.title = element_text(size = 10), axis.title.x = element_blank())
  plot
}


# Edited function for the dynamic model calibrated for Olive ####
Calib_Dynamic_Model <- function (HourTemp, summ = TRUE, E0 = 3.240997e+03, E1 = 9.921809e+03, 
          A0 = 6.867300e+03, A1 = 5.939913e+13, slope = 3.555938e+00, Tf = 280.642625) 
{
  if (missing(HourTemp)) {
    stop("HourTemp must be present! Aborting.")
  }
  TK <- HourTemp + 273
  aa <- A0/A1
  ee <- E1 - E0
  sr <- exp(slope * Tf * (TK - Tf)/TK)
  xi <- sr/(1 + sr)
  xs <- aa * exp(ee/TK)
  eak1 <- exp(-A1 * exp(-E1/TK))
  x = 0
  for (l in c(2:length(HourTemp))) {
    S <- x[l - 1]
    if (x[l - 1] >= 1) {
      S <- S * (1 - xi[l - 2])
    }
    x[l] <- xs[l - 1] - (xs[l - 1] - S) * eak1[l - 1]
  }
  delta <- rep(0, length(HourTemp))
  ii <- which(x >= 1)
  delta[ii] <- x[ii] * xi[ii - 1]
  if (summ) 
    return(cumsum(delta))
  return(delta)
}

# Function for chill delay plot
plot_chill_delay <- function (past_observed, past_simulated, future_data, metric, 
          axis_label) 
{
rng <- range(-5, past_simulated[[metric]], 
             future_data[[metric]],na.rm = T)

past_plot<-ggplot() +
  geom_boxplot(data = past_simulated,
               aes_string("as.numeric(Scen_year)",metric,group="Scen_year"),
               fill="skyblue") +
  geom_hline(aes(yintercept = 0),color = "blue1", linetype = "solid")+
  scale_y_continuous(limits = c(-5, round(round(1.1*rng[2])))) +
  labs(x = "Year", y = axis_label) +
  facet_grid(~ scen_year) +
  theme_bw(base_size = 15) +  
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle=45, hjust=1)) +
  geom_point(data = past_observed,
             aes_string("Year",metric),
             col="blue")


future_plot_list <- list()
for (y in c(2050, 2085)) {
  future_plot_list[[which(y == c(2050, 2085))]] <- ggplot(data = future_data[which(future_data$Year == y), ]) + 
    geom_boxplot(aes_string("GCM", metric, fill = "GCM")) +
    facet_wrap(vars(RCP)) +
    geom_hline(aes(yintercept = 0),color = "blue1", linetype = "solid")+
    scale_x_discrete(labels = NULL, expand = expansion(add = 1)) + 
    scale_y_continuous(limits = c(-5, round(round(1.1 * rng[2])))) + 
    geom_text_npc(aes(npcx = "center", npcy = "top", label = Year), size = 5) + 
    theme_bw(base_size = 15) + 
    theme(axis.ticks.y = element_blank(), axis.text = element_blank(), 
          axis.title = element_blank(), legend.position = "bottom", 
          legend.margin = margin(0, 0, 0, 0, "cm"), legend.background = element_rect(), 
          strip.background = element_blank(), strip.text = element_text(face = "bold"), 
          legend.box.spacing = unit(0, "cm"), plot.subtitle = element_text(hjust = 0.5, 
                                                                           vjust = -1, size = 15 * 1.05, face = "bold"))
}

plot <- (past_plot + future_plot_list + plot_layout(guides = "collect", 
                                                    widths = c(1, rep(1.8, length(future_plot_list))))) & 
  theme(legend.position = "bottom", legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10), axis.title.x = element_blank())
plot
}

# Plot chill delay for low chill cultivars
plot_lowchill_delay <- function (past_observed, past_simulated, future_data, metric, 
                              axis_label) {
  rng <- range(-5, past_simulated[[metric]], 
               future_data[[metric]],na.rm = TRUE)
  
  past_plot <- ggplot() +
    geom_boxplot(data = past_simulated,
                 aes_string("as.numeric(Scen_year)",metric,group="Scen_year"),
                 fill="skyblue") +
    geom_hline(aes(yintercept = 0),color = "blue1", linetype = "solid") +
    scale_y_continuous(limits = c(-5, round(round(1.1*rng[2])))) +
    labs(x = "Year", y = axis_label) +
    facet_grid(~ scen_year,labeller = labeller(Scenario = "")) +
    theme_bw(base_size = 15) +  
    theme(strip.background = element_blank(),
          strip.text = element_blank(),  # Remove facet labels above
          axis.text.x = element_text(angle=45, hjust=1)) +
    geom_point(data = past_observed,
               aes_string("Year",metric),
               col="blue")
  
  future_plot_list <- list()
  for (y in c(2050, 2085)) {
    future_plot_list[[which(y == c(2050, 2085))]] <- ggplot(data = future_data[which(future_data$Year == y), ]) + 
      geom_boxplot(aes_string("GCM", metric, fill = "GCM")) +
      facet_wrap(vars(RCP)) +
      geom_hline(aes(yintercept = 0),color = "blue1", linetype = "solid") +
      scale_x_discrete(labels = NULL, expand = expansion(add = 1)) + 
      scale_y_continuous(limits = c(-5, round(round(1.1 * rng[2])))) + 
      geom_text_npc(aes(npcx = "center", npcy = "top", label = Year), size = 5) + 
      theme_bw(base_size = 15) + 
      theme(axis.ticks.y = element_blank(), axis.text = element_blank(), 
            axis.title = element_blank(), legend.position = "bottom", 
            legend.margin = margin(0, 0, 0, 0, "cm"), legend.background = element_rect(), 
            strip.background = element_blank(), strip.text = element_blank(), 
            legend.box.spacing = unit(0, "cm"), plot.subtitle = element_text(hjust = 0.5, 
                                                                               vjust = -1, size = 15 * 1.05, face = "bold"))
  }
  
  plot <- (past_plot + future_plot_list + plot_layout(guides = "collect", 
                                                      widths = c(1, rep(1.8, length(future_plot_list))))) & 
    theme(legend.position = "bottom", legend.text = element_text(size = 8), 
          legend.title = element_text(size = 10), axis.title.x = element_blank())
  plot
}

# Plot chill delay for mid chill cultivars
plot_midchill_delay <- function (past_observed, past_simulated, future_data, metric, 
                              axis_label) {
  rng <- range(-5, past_simulated[[metric]], 
               future_data[[metric]],na.rm = TRUE)
  
  past_plot <- ggplot() +
    geom_boxplot(data = past_simulated,
                 aes_string("as.numeric(Scen_year)",metric,group="Scen_year"),
                 fill="skyblue") +
    geom_hline(aes(yintercept = 0),color = "blue1", linetype = "solid") +
    scale_y_continuous(limits = c(-5, round(round(1.1*rng[2])))) +
    labs(x = "Year", y = axis_label) +
    facet_grid(~ scen_year,labeller = labeller(Scenario = "")) +
    theme_bw(base_size = 15) +  
    theme(strip.background = element_blank(),
          strip.text = element_blank(),  # Remove facet labels above
          axis.text.x = element_blank()) +
    geom_point(data = past_observed,
               aes_string("Year",metric),
               col="blue")
  
  future_plot_list <- list()
  for (y in c(2050, 2085)) {
    future_plot_list[[which(y == c(2050, 2085))]] <- ggplot(data = future_data[which(future_data$Year == y), ]) + 
      geom_boxplot(aes_string("GCM", metric, fill = "GCM")) +
      facet_wrap(vars(RCP)) +
      geom_hline(aes(yintercept = 0),color = "blue1", linetype = "solid") +
      scale_x_discrete(labels = NULL, expand = expansion(add = 1)) + 
      scale_y_continuous(limits = c(-5, round(round(1.1 * rng[2])))) + 
      geom_text_npc(aes(npcx = "center", npcy = "top", label = Year), size = 5) + 
      theme_bw(base_size = 15) + 
      theme(axis.ticks.y = element_blank(), axis.text = element_blank(), 
            axis.title = element_blank(), legend.position = "bottom", 
            legend.margin = margin(0, 0, 0, 0, "cm"), legend.background = element_rect(), 
            strip.background = element_blank(), strip.text = element_blank(), 
            legend.box.spacing = unit(0, "cm"), plot.subtitle = element_text(hjust = 0.5, 
                                                                               vjust = -1, size = 15 * 1.05, face = "bold"))
  }
  
  plot <- (past_plot + future_plot_list + plot_layout(guides = "collect", 
                                                      widths = c(1, rep(1.8, length(future_plot_list))))) & 
    theme(legend.position = "bottom", legend.text = element_text(size = 8), 
          legend.title = element_text(size = 10), axis.title.x = element_blank())
  plot
}

# Plot chill delay for high chill cultivars
plot_highchill_delay <- function (past_observed, past_simulated, future_data, metric, 
          axis_label) 
{
rng <- range(-5, past_simulated[[metric]], 
             future_data[[metric]],na.rm = T)

past_plot<-ggplot() +
  geom_boxplot(data = past_simulated,
               aes_string("as.numeric(Scen_year)",metric,group="Scen_year"),
               fill="skyblue") +
  geom_hline(aes(yintercept = 0),color = "blue1", linetype = "solid")+
  scale_y_continuous(limits = c(-5, round(round(1.1*rng[2])))) +
  labs(x = "Year", y = axis_label) +
  facet_grid(~ scen_year) +
  theme_bw(base_size = 15) +  
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_blank()) +
  geom_point(data = past_observed,
             aes_string("Year",metric),
             col="blue")


future_plot_list <- list()
for (y in c(2050, 2085)) {
  future_plot_list[[which(y == c(2050, 2085))]] <- ggplot(data = future_data[which(future_data$Year == y), ]) + 
    geom_boxplot(aes_string("GCM", metric, fill = "GCM")) +
    facet_wrap(vars(RCP)) +
    geom_hline(aes(yintercept = 0),color = "blue1", linetype = "solid")+
    scale_x_discrete(labels = NULL, expand = expansion(add = 1)) + 
    scale_y_continuous(limits = c(-5, round(round(1.1 * rng[2])))) + 
    geom_text_npc(aes(npcx = "center", npcy = "top", label = Year), size = 5) + 
    theme_bw(base_size = 15) + 
    theme(axis.ticks.y = element_blank(), axis.text = element_blank(), 
          axis.title = element_blank(), legend.position = "bottom", 
          legend.margin = margin(0, 0, 0, 0, "cm"), legend.background = element_rect(), 
          strip.background = element_blank(), strip.text = element_text(face = "bold"), 
          legend.box.spacing = unit(0, "cm"), plot.subtitle = element_text(hjust = 0.5, 
                                                                           vjust = -1, size = 15 * 1.05, face = "bold"))
}

plot <- (past_plot + future_plot_list + plot_layout(guides = "collect", 
                                                    widths = c(1, rep(1.8, length(future_plot_list))))) & 
  theme(legend.position = "bottom", legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10), axis.title.x = element_blank())
plot
}
