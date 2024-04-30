# Koroneiki ####

# Forcing plot ####
forcing_Koroneiki <- forcing_vf %>%
  rename(Unforced=Poids_frais)%>%
  rename(Forced=Poids_frais_f)%>%
  pivot_longer(cols = c(Unforced, Forced),    
               names_to = "Treatment",        
               values_to = "Weight")%>%
  filter(Cultivar=="Koroneiki")

forcing_Koroneiki <- na.omit(forcing_Koroneiki)
forcing_Koroneiki$Date <- as.Date(forcing_Koroneiki$Date)

# Visualization
# The Plot
Koroneiki <- ggline(forcing_Koroneiki, x = "Date", y = "Weight", add = "mean_se",
       color = "Treatment",facet.by = "Year")+
  stat_compare_means(aes(group = Treatment), label = "p.signif",paired = T,
                     method = "t.test",vjust = 0.5)

Koroneiki <- facet(Koroneiki,facet.by = "Year",scales = "free",panel.labs.font = list(face = "bold"))+
    labs(x="Sampling date", y="Floral bud fresh weights (mg)")+
    scale_color_manual(values=c("cornflowerblue", "coral"),
                       name="",
                       breaks=c("Unforced", "Forced"),
                       labels=c("**Unforced**", "**Forced**"))+
    ggtitle("Koroneiki")+
    theme(plot.title = element_text(color="black", size=14, face="bold.italic",
                                    hjust = 0.5,vjust = 0.5))+
    theme(axis.title.x = element_text(face="bold", size=12))+
    theme(axis.title.y = element_text(face="bold", size=12))+
    theme(legend.text = element_markdown())+
  scale_x_date(breaks = breaks, date_labels = "%b %d")

Koroneiki <- Koroneiki + geom_vline(xintercept = as.Date(c("2021-02-25", "2022-02-22")), 
                linetype = "dotdash", color = "green4",alpha=0.5,lwd=1)
Koroneiki

# Validation version 1 ####

# Validation of the fitted PM PhenoFlex
# Validation Version 1; from the first level of calibration without forcing

# Season list selection starting from the season onset set by PLS output (Abou-Saaid & al 2022)
as.POSIXlt(c("26.12"), format = "%d.%m")$yday+1
as.Date(360, format="%d.%m",origin = "01.01")-1

valid_season_list_26Dec <- list()

for (i in c(1:6)) {
  num_rows <- nrow(valid_season_list[[i]])
  valid_season_list_26Dec[[i]] <- valid_season_list[[i]][1321:num_rows,]
}

# Validation data version 1
valid_df_koroneiki_v1 <- df_Koroneiki[1:6,]

# Validation
for (i in 1 : 6) {
  valid_df_koroneiki_v1[i, "Predicted"] <- PhenoFlex_GDHwrapper(valid_season_list_26Dec[[i]], Pheno_fit$par)
}

# Estimate the RMSEP
RMSEP_valid_v1_koroneiki <- RMSEP(valid_df_koroneiki_v1$Predicted, valid_df_koroneiki_v1$pheno, na.rm = TRUE)
RMSEP_valid_v1_koroneiki

# Compute the MAE
mae_valid_v1_koroneiki <- mean(abs(valid_df_koroneiki_v1$pheno - valid_df_koroneiki_v1$Predicted), na.rm = TRUE)
mae_valid_v1_koroneiki

# Observed 'yc' and 'zc' ####
# To visualize the actual yc and zc starting from the season onset given by PLS (Abou-Saaid et., 2022)

# Season 2020_2021 ####
season_data_2021_26Dec<-data[iSeason_2021[[1]][1321:length_iSeason_2021],]

# Fitted yc and zc of PM
yc=5.800000e+01
zc=7.850000e+02

res_2021_26Dec <- PhenoFlex(temp=season_data_2021_26Dec$Temp,
                            times=c(1: length(season_data_2021_26Dec$Temp)),
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

seasontemps_2021_26Dec<-season_data_2021_26Dec
seasontemps_2021_26Dec[,"x"]<-res_2021_26Dec$x
seasontemps_2021_26Dec[,"y"]<-res_2021_26Dec$y
seasontemps_2021_26Dec[,"z"]<-res_2021_26Dec$z
seasontemps_2021_26Dec<-add_date(seasontemps_2021_26Dec)

Actual_yc_koroneiki_2021<-seasontemps_2021_26Dec$y[which(seasontemps_2021_26Dec$Date>="2021-02-25 00:00:00")[1]]
Actual_yc_koroneiki_2021

# Season 2021_2022 ####
season_data_2022_26Dec<-data[iSeason_2022[[1]][1321:length_iSeason_2022],]

res_2022_26Dec <- PhenoFlex(temp=season_data_2022_26Dec$Temp,
                            times=c(1: length(season_data_2022_26Dec$Temp)),
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

seasontemps_2022_26Dec<-season_data_2022_26Dec
seasontemps_2022_26Dec[,"x"]<-res_2022_26Dec$x
seasontemps_2022_26Dec[,"y"]<-res_2022_26Dec$y
seasontemps_2022_26Dec[,"z"]<-res_2022_26Dec$z
seasontemps_2022_26Dec <-add_date(seasontemps_2022_26Dec)

Actual_yc_koroneiki_2022<-seasontemps_2022_26Dec$y[which(seasontemps_2022_26Dec$Date>="2022-02-22 00:00:00")[1]]
Actual_yc_koroneiki_2022

# Calibration of yc and zc ####
# Calibration of yc and zc according to the forcing output

# Improving the calibration process with different values of yc, zc
# All possible combinations
# Initialize empty vectors to store the combinations
yc_values <- vector("integer")
zc_values <- vector("integer")

# Generating combinations for vector1, yc values according to forcing test
for (i in seq(Actual_yc_koroneiki_2022, Actual_yc_koroneiki_2021,by = 0.1)) {
  yc_values <- c(yc_values, i)
}

# Generating combinations for vector2 (700 to 1200, moving by 5 units)
for (j in seq(700, 1200, by = 5)) {
  zc_values <- c(zc_values, j)
}

# Forcing season list January
season_list_forcing_26Dec <- list()

for (i in c(1:2)) {
  num_rows <- nrow(season_list_21_22[[i]])
  season_list_forcing_26Dec[[i]] <- season_list_21_22[[i]][1321:num_rows,]
}

# Run the loop to get the best combination of yc and zc. The choice of the best combination is based
# smallest mean absolute error between the two years.

lowest_mae <- Inf  # Initialize with a large value
calibration_koroneiki <- NULL

for (yc in yc_values) {
  for (zc in zc_values) {
    
    # Model parameters here
    Pheno_fit_recalib <- c(yc, zc, 7.992534e-01, 2.681513e+01, 3.240997e+03, 9.921809e+03, 6.867300e+03, 5.939913e+13, 7.492625e+00, 3.427934e+01, 2.959986e+00, 3.555938e+00)
    
    forc_Koroneiki <- df_Koroneiki[df_Koroneiki$Year %in% forcing_years, ]
    for (i in 1:nrow(forc_Koroneiki)) {
      num_rows <- nrow(season_list_forcing_26Dec[[i]])
      forc_Koroneiki[i, "Predicted"] <- PhenoFlex_GDHwrapper(season_list_forcing_26Dec[[i]], Pheno_fit_recalib)
    }
    
    Observed <- c(forc_Koroneiki[1,]$pheno,forc_Koroneiki[2,]$pheno)
    predicted <- c(forc_Koroneiki[1,]$Predicted, forc_Koroneiki[2,]$Predicted)
    mae <- mean(abs(Observed - predicted))
    
    # Update if the current combination has a lower MAE
    if (mae < lowest_mae) {
      lowest_mae <- mae
      calibration_koroneiki <- c(yc = yc, zc = zc, mae = lowest_mae)
    }
  }
}
print(calibration_koroneiki)

# Validation version 2 ####
# Re-calibrated parameters
Pheno_fit_koroneiki <- c(calibration_koroneiki[1],calibration_koroneiki[2], 7.992534e-01, 2.681513e+01, 3.240997e+03, 9.921809e+03, 6.867300e+03, 5.939913e+13, 7.492625e+00, 3.427934e+01, 2.959986e+00, 3.555938e+00)

# Validation data version 2
valid_df_koroneiki_v2 <- df_Koroneiki[1:6,]

# Validation
for (i in 1 : 6) {
  valid_df_koroneiki_v2[i, "Predicted"] <- PhenoFlex_GDHwrapper(valid_season_list_26Dec[[i]], Pheno_fit_koroneiki)
}

# Estimate the RMSEP
RMSEP_valid_v2_koroneiki <- RMSEP(valid_df_koroneiki_v2$Predicted, valid_df_koroneiki_v2$pheno, na.rm = TRUE)
RMSEP_valid_v2_koroneiki

# Compute the MAE
mae_valid_v2_koroneiki <- mean(abs(valid_df_koroneiki_v2$pheno - valid_df_koroneiki_v2$Predicted), na.rm = TRUE)
mae_valid_v2_koroneiki

##################
# Estimation of chill and heat using PhenoFlex and forcing test
# Fitting yc and zc to 8 years of bloom dates

# Initialize empty vectors to store the combinations
yc_values <- vector("integer")
zc_values <- vector("integer")

# Generating combinations for vector1
Actual_yc_koroneiki_2021
Actual_yc_koroneiki_2022

for (i in seq(Actual_yc_koroneiki_2022, Actual_yc_koroneiki_2021,by = 0.1)) {
  yc_values <- c(yc_values, i)
}

# Generating combinations for vector2 
for (j in seq(600, 1200, by = 5)) {
  zc_values <- c(zc_values, j)
}

# Then I generate a list of seasons 
calib_season_list_26Dec <- list()

for (i in c(1:8)) {
  num_rows <- nrow(calib_season_list[[i]])
  calib_season_list_26Dec[[i]] <- calib_season_list[[i]][1321:num_rows,]
}

# Run the loop to get the best combination of yc and zc. The choice of the best combination is based
# smallest RMSE.

lowest_RMSE <- Inf  # Initialize with a large value
chill_heat_koroneiki <- NULL

for (yc in yc_values) {
  for (zc in zc_values) {
    
    # Model parameters here
    Pheno_fit_recalib <- c(yc, zc, 7.992534e-01, 2.681513e+01, 3.240997e+03, 9.921809e+03, 6.867300e+03, 5.939913e+13, 7.492625e+00, 3.427934e+01, 2.959986e+00, 3.555938e+00)
    
    calib_Koroneiki <- df_Koroneiki
    for (i in 1:8) {
      calib_Koroneiki[i, "Predicted"] <- PhenoFlex_GDHwrapper(calib_season_list_26Dec[[i]], Pheno_fit_recalib)
    }
    
    RMSE <- RMSEP(calib_Koroneiki$Predicted, calib_Koroneiki$pheno, na.rm = TRUE)
    
    # Update if the current combination has a lower MAE
    if (RMSE < lowest_RMSE) {
      lowest_RMSE <- RMSE
      chill_heat_koroneiki <- c(yc = yc, zc = zc, RMSE = lowest_RMSE)
    }
  }
}
print(chill_heat_koroneiki)

# Application of the default model
calib_Koroneiki <- df_Koroneiki
for (i in 1:8) {
  calib_Koroneiki[i, "Predicted"] <- PhenoFlex_GDHwrapper(calib_season_list_26Dec[[i]], Pheno_fit$par)
}

RMSE_koroneiki_v0 <- RMSEP(calib_Koroneiki$Predicted, calib_Koroneiki$pheno, na.rm = TRUE)
RMSE_koroneiki_v0
