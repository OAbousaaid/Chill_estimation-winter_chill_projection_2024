# Application of the fitted model
# Upload packages
library(tidyr)
library(tidyverse)
library(dplyr)
library(multcomp)
library(ggplot2)
library(scales)
library(ggpubr)
library(ggtext)
library(readxl)
library(patchwork)

# Upload forcing data
forcing <- read_excel("Data/Res_forc_vf.xlsx", sheet = "Feuil_vf",
                      col_types = c("date","text","text","text","numeric","numeric"))
forcing<- as.data.frame(forcing)

forcing_vf <- forcing[,c(1,3:6)]

# Upload FFD data for all cultivars
allvar_bloom <- read_excel("Data/Bloom_9_cultivars.xlsx", sheet = "bloom")

# Get the column names except the first column
second_cols <- names(allvar_bloom)[-1]

# Loop over yc and zc values
for (col_name in second_cols) {
  
  # Create a new data frame with two columns
  valid_df <- data.frame(Year = allvar_bloom[, 1], pheno = allvar_bloom[, col_name])
  
  # Rename the second column to "pheno"
  colnames(valid_df)[2] <- "pheno"
  
  # Assign the new data frame to a variable with a unique name
  assign(paste("df_", col_name, sep = ""), valid_df)
}

# Temperature data
data <- read_excel("HT2013_2022.xlsx")
data <- as.data.frame(data)

# Sabatera ####

# Forcing Plot ####
forcing_Sabatera <- forcing_vf %>%
  rename(Unforced=Poids_frais)%>%
  rename(Forced=Poids_frais_f)%>%
  pivot_longer(cols = c(Unforced, Forced),    
               names_to = "Treatment",        
               values_to = "Weight")%>%
  filter(Cultivar=="Sabatera")

forcing_Sabatera <- na.omit(forcing_Sabatera)
forcing_Sabatera$Date <- as.Date(forcing_Sabatera$Date)

# Visualization

# The Plot
Sabatera <- ggline(forcing_Sabatera, x = "Date", y = "Weight", add = "mean_se",
                   color = "Treatment",facet.by = "Year")+
  stat_compare_means(aes(group = Treatment), label = "p.signif",paired = T,
                     method = "t.test",vjust = 0.5)

# X axis start date and break
breaks <- seq(as.Date("2021-01-01"), as.Date("2022-12-31"), by = "17 days")

Sabatera <- facet(Sabatera,facet.by = "Year",scales = "free",panel.labs.font = list(face = "bold"))+
  labs(x="Sampling date", y="Floral bud fresh weights (mg)")+
  scale_color_manual(values=c("cornflowerblue", "coral"),
                     name="",
                     breaks=c("Unforced", "Forced"),
                     labels=c("**Unforced**", "**Forced**"))+
  ggtitle("Sabatera")+
  theme(plot.title = element_text(color="black", size=14, face="bold.italic",
                                  hjust = 0.5,vjust = 0.5))+
  theme(axis.title.x = element_text(face="bold", size=12))+
  theme(axis.title.y = element_text(face="bold", size=12))+
  theme(legend.text = element_markdown())+
  scale_x_date(breaks = breaks, date_labels = "%b %d")


Sabatera <- Sabatera + geom_vline(xintercept = as.Date(c("2021-02-20", "2022-02-01")), 
                                  linetype = "dotdash", color = "green4",alpha=0.5,lwd=1)
Sabatera

# Validation version 1 ####

# Validation of the fitted PM PhenoFlex
# Validation Version 1; from the first level of calibration without forcing

# Season list selection starting from the season onset set by PLS output (Abou-Saaid & al 2022)
as.POSIXlt(c("1.1"), format = "%d.%m")$yday+1
as.Date(1, format="%d.%m",origin = "01.01")-1

valid_season_list_January <- list()

for (i in c(1:6)) {
  num_rows <- nrow(valid_season_list[[i]])
  valid_season_list_January[[i]] <- valid_season_list[[i]][1464:num_rows,]
}

# Validation data version 1
valid_df_sabatera_v1 <- df_Sabatera[1:6,]

# Validation
for (i in 1 : 6) {
  valid_df_sabatera_v1[i, "Predicted"] <- PhenoFlex_GDHwrapper(valid_season_list_January[[i]], Pheno_fit$par)
}

# Estimate the RMSEP
RMSEP_valid_v1_sabatera <- RMSEP(valid_df_sabatera_v1$Predicted, valid_df_sabatera_v1$pheno, na.rm = TRUE)
RMSEP_valid_v1_sabatera

# Compute the MAE
mae_valid_v1_sabatera <- mean(abs(valid_df_sabatera_v1$pheno - valid_df_sabatera_v1$Predicted), na.rm = TRUE)
mae_valid_v1_sabatera

# Observed 'yc' and 'zc' ####
# To visualize the actual yc and zc starting from the season onset given by PLS (Abou-Saaid et., 2022)
as.POSIXlt(c("1.1"), format = "%d.%m")$yday+1
as.Date(1, format="%d.%m",origin = "01.01")-1

# Season 2020_2021 ####
iSeason_2021 <- genSeason(data,
                          mrange = c(11, 6),
                          years=c(2021))

length_iSeason_2021 <- length(iSeason_2021[[1]])

season_data_2021_January<-data[iSeason_2021[[1]][1464:length_iSeason_2021],]

# Fitted yc and zc of PM
yc=5.800000e+01
zc=7.850000e+02

res_2021_January <- PhenoFlex(temp=season_data_2021_January$Temp,
                              times=c(1: length(season_data_2021_January$Temp)),
                              stopatzc=T,basic_output=F,
                              yc=yc, 
                              zc=zc, 
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

seasontemps_2021_January<-season_data_2021_January
seasontemps_2021_January[,"x"]<-res_2021_January$x
seasontemps_2021_January[,"y"]<-res_2021_January$y
seasontemps_2021_January[,"z"]<-res_2021_January$z
seasontemps_2021_January<-add_date(seasontemps_2021_January)

Actual_yc_sabatera_2021<-seasontemps_2021_January$y[which(seasontemps_2021_January$Date>="2021-02-20 00:00:00")[1]]
Actual_yc_sabatera_2021

# Season 2021_2022 ####
iSeason_2022 <- genSeason(data,
                          mrange = c(11, 6),
                          years=c(2022))

length_iSeason_2022 <- length(iSeason_2022[[1]])

season_data_2022_January<-data[iSeason_2022[[1]][1464:length_iSeason_2022],]

res_2022_January <- PhenoFlex(temp=season_data_2022_January$Temp,
                              times=c(1: length(season_data_2022_January$Temp)),
                              stopatzc=T,basic_output=F,
                              yc=yc, 
                              zc=zc, 
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

seasontemps_2022_January<-season_data_2022_January
seasontemps_2022_January[,"x"]<-res_2022_January$x
seasontemps_2022_January[,"y"]<-res_2022_January$y
seasontemps_2022_January[,"z"]<-res_2022_January$z
seasontemps_2022_January <-add_date(seasontemps_2022_January)

Actual_yc_sabatera_2022<-seasontemps_2022_January$y[which(seasontemps_2022_January$Date>="2022-02-01 00:00:00")[1]]
Actual_yc_sabatera_2022

# Calibration of yc and zc ####
# Calibration of yc and zc according to the forcing output

# Improving the calibration process with different values of yc, zc
# All possible combinations
# Initialize empty vectors to store the combinations
yc_values <- vector("integer")
zc_values <- vector("integer")

# Generating combinations for vector1, yc values according to forcing test
for (i in seq(Actual_yc_sabatera_2022,Actual_yc_sabatera_2021,by = 0.1)) {
  yc_values <- c(yc_values, i)
}

# Generating combinations for vector2 (700 to 1200, moving by 5 units)
for (j in seq(700, 1200, by = 5)) {
  zc_values <- c(zc_values, j)
}

# Forcing season list January
season_list_forcing_January <- list()

for (i in c(1:2)) {
  num_rows <- nrow(season_list_21_22[[i]])
  season_list_forcing_January[[i]] <- season_list_21_22[[i]][1464:num_rows,]
}

# Run the loop to get the best combination of yc and zc. The choice of the best combination is based
# smallest mean absolute error between the two years.

lowest_mae <- Inf  # Initialize with a large value
calibration_sabatera <- NULL

for (yc in yc_values) {
  for (zc in zc_values) {
    
    # Model parameters here
    Pheno_fit_recalib <- c(yc, zc, 7.992534e-01, 2.681513e+01, 3.240997e+03, 9.921809e+03, 6.867300e+03, 5.939913e+13, 7.492625e+00, 3.427934e+01, 2.959986e+00, 3.555938e+00)
    
    forc_Sabatera <- df_Sabatera[df_Sabatera$Year %in% forcing_years, ]
    for (i in 1:nrow(forc_Sabatera)) {
      num_rows <- nrow(season_list_forcing_January[[i]])
      forc_Sabatera[i, "Predicted"] <- PhenoFlex_GDHwrapper(season_list_forcing_January[[i]], Pheno_fit_recalib)
    }
    
    Observed <- c(forc_Sabatera[1,]$pheno,forc_Sabatera[2,]$pheno)
    predicted <- c(forc_Sabatera[1,]$Predicted, forc_Sabatera[2,]$Predicted)
    mae <- mean(abs(Observed - predicted))
    
    # Update if the current combination has a lower MAE
    if (mae < lowest_mae) {
      lowest_mae <- mae
      calibration_sabatera <- c(yc = yc, zc = zc, mae = lowest_mae)
    }
  }
}
print(calibration_sabatera)

# Validation version 2 ####
# calibrated yc and zc
Pheno_fit_sabatera <- c(calibration_sabatera[1], calibration_sabatera[2], 7.992534e-01, 2.681513e+01, 3.240997e+03, 9.921809e+03, 6.867300e+03, 5.939913e+13, 7.492625e+00, 3.427934e+01, 2.959986e+00, 3.555938e+00)

# Validation data version 2
valid_df_Sabatera_v2 <- df_Sabatera[1:6,]

# Validation
for (i in 1 : 6) {
  valid_df_Sabatera_v2[i, "Predicted"] <- PhenoFlex_GDHwrapper(valid_season_list_January[[i]], Pheno_fit_sabatera)
}

# Estimate the RMSEP
RMSEP_valid_v2_sabatera <- RMSEP(valid_df_Sabatera_v2$Predicted, valid_df_Sabatera_v2$pheno, na.rm = TRUE)
RMSEP_valid_v2_sabatera

# Compute the MAE
mae_valid_v2_sabatera <- mean(abs(valid_df_Sabatera_v2$pheno - valid_df_Sabatera_v2$Predicted), na.rm = TRUE)
mae_valid_v2_sabatera

##################
# Estimation of chill and heat using PhenoFlex and forcing test
# Fitting yc and zc to 8 years of bloom dates

# Initialize empty vectors to store the combinations
yc_values <- vector("integer")
zc_values <- vector("integer")

# Generating combinations for vector1
Actual_yc_sabatera_2021
Actual_yc_sabatera_2022

for (i in seq(Actual_yc_sabatera_2022, Actual_yc_sabatera_2021,by = 0.1)) {
  yc_values <- c(yc_values, i)
}

# Generating combinations for vector2
for (j in seq(600, 1200, by = 5)) {
  zc_values <- c(zc_values, j)
}

# Then I generate a list of seasons with weather data for the forcing calibration procedure (2021 and 2022)
calib_season_list_January <- list()

for (i in c(1:8)) {
  num_rows <- nrow(calib_season_list[[i]])
  calib_season_list_January[[i]] <- calib_season_list[[i]][1464:num_rows,]
}

# Run the loop to get the best combination of yc and zc. The choice of the best combination is based
# smallest mean absolute error between the two years.

lowest_RMSE <- Inf  # Initialize with a large value
chill_heat_sabatera <- NULL

for (yc in yc_values) {
  for (zc in zc_values) {
    
    # Model parameters here
    Pheno_fit_recalib <- c(yc, zc, 7.992534e-01, 2.681513e+01, 3.240997e+03, 9.921809e+03, 6.867300e+03, 5.939913e+13, 7.492625e+00, 3.427934e+01, 2.959986e+00, 3.555938e+00)
    
    calib_Sabatera <- df_Sabatera
    for (i in 1:8) {
      calib_Sabatera[i, "Predicted"] <- PhenoFlex_GDHwrapper(calib_season_list_January[[i]], Pheno_fit_recalib)
    }
    
    RMSE <- RMSEP(calib_Sabatera$Predicted, calib_Sabatera$pheno, na.rm = TRUE)
    
    # Update if the current combination has a lower MAE
    if (RMSE < lowest_RMSE) {
      lowest_RMSE <- RMSE
      chill_heat_sabatera <- c(yc = yc, zc = zc, RMSE = lowest_RMSE)
    }
  }
}
print(chill_heat_sabatera)

# Default model application for 8 years of data
calib_Sabatera <- df_Sabatera
for (i in 1:8) {
  calib_Sabatera[i, "Predicted"] <- PhenoFlex_GDHwrapper(calib_season_list_January[[i]], Pheno_fit$par)
}

RMSE_sabatera_v0 <- RMSEP(calib_Sabatera$Predicted, calib_Sabatera$pheno, na.rm = TRUE)
RMSE_sabatera_v0

