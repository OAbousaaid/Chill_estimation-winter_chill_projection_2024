# Sevillenca ####

# Forcing plot ####
forcing_Sevillenca <- forcing_vf %>%
  rename(Unforced=Poids_frais)%>%
  rename(Forced=Poids_frais_f)%>%
  pivot_longer(cols = c(Unforced, Forced),    
               names_to = "Treatment",        
               values_to = "Weight")%>%
  filter(Cultivar=="Sevillenca")

forcing_Sevillenca <- na.omit(forcing_Sevillenca)
forcing_Sevillenca$Date <- as.Date(forcing_Sevillenca$Date)

# Visualization

# The Plot
Sevillenca <- ggline(forcing_Sevillenca, x = "Date", y = "Weight", add = "mean_se",
                     color = "Treatment",facet.by = "Year")+
  stat_compare_means(aes(group = Treatment), label = "p.signif",paired = T,
                     method = "t.test",vjust = 0.5)

# X axis start date and break
breaks <- seq(as.Date("2021-01-01"), as.Date("2022-12-31"), by = "17 days")

Sevillenca <- facet(Sevillenca,facet.by = "Year",scales = "free",panel.labs.font = list(face = "bold"))+
  labs(x="Sampling date", y="Floral bud fresh weights (mg)")+
  scale_color_manual(values=c("cornflowerblue", "coral"),
                     name="",
                     breaks=c("Unforced", "Forced"),
                     labels=c("**Unforced**", "**Forced**"))+
  ggtitle("Sevillenca")+
  theme(plot.title = element_text(color="black", size=14, face="bold.italic",
                                  hjust = 0.5,vjust = 0.5))+
  theme(axis.title.x = element_text(face="bold", size=12))+
  theme(axis.title.y = element_text(face="bold", size=12))+
  theme(legend.text = element_markdown())+
  scale_x_date(breaks = breaks, date_labels = "%b %d")


Sevillenca <- Sevillenca + geom_vline(xintercept = as.Date(c("2021-02-01", "2022-02-08")), 
                                      linetype = "dotdash", color = "green4",alpha=0.5, lwd =1)
Sevillenca

# Validation version 1 ####

# Validation of the fitted PM PhenoFlex
# Validation Version 1; from the first level of calibration without forcing

# Season list selection starting from the season onset set by PLS output (Abou-Saaid & al 2022)
as.POSIXlt(c("30.12"), format = "%d.%m")$yday+1
as.Date(364, format="%d.%m",origin = "01.01")-1

valid_season_list_30Dec <- list()

for (i in c(1:6)) {
  num_rows <- nrow(valid_season_list[[i]])
  valid_season_list_30Dec[[i]] <- valid_season_list[[i]][1417:num_rows,]
}

# Validation data version 1
valid_df_sevillenca_v1 <- df_Sevillenca[1:6,]

# Validation
for (i in 1 : 6) {
  valid_df_sevillenca_v1[i, "Predicted"] <- PhenoFlex_GDHwrapper(valid_season_list_30Dec[[i]], Pheno_fit$par)
}

# Estimate the RMSEP
RMSEP_valid_v1_sevillenca <- RMSEP(valid_df_sevillenca_v1$Predicted, valid_df_sevillenca_v1$pheno, na.rm = TRUE)
RMSEP_valid_v1_sevillenca

# Compute the MAE
mae_valid_v1_sevillenca <- mean(abs(valid_df_sevillenca_v1$pheno - valid_df_sevillenca_v1$Predicted), na.rm = TRUE)
mae_valid_v1_sevillenca

# Observed 'yc' and 'zc' ####
# To visualize the actual yc and zc starting from the season onset given by PLS (Abou-Saaid et., 2022)

# Season 2020_2021 ####
season_data_2021_30Dec<-data[iSeason_2021[[1]][1417:length_iSeason_2021],]

# Fitted yc and zc of PM
yc=5.800000e+01
zc=7.850000e+02

res_2021_30Dec <- PhenoFlex(temp=season_data_2021_30Dec$Temp,
                              times=c(1: length(season_data_2021_30Dec$Temp)),
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

seasontemps_2021_30Dec<-season_data_2021_30Dec
seasontemps_2021_30Dec[,"x"]<-res_2021_30Dec$x
seasontemps_2021_30Dec[,"y"]<-res_2021_30Dec$y
seasontemps_2021_30Dec[,"z"]<-res_2021_30Dec$z
seasontemps_2021_30Dec<-add_date(seasontemps_2021_30Dec)

Actual_yc_sevillenca_2021<-seasontemps_2021_30Dec$y[which(seasontemps_2021_30Dec$Date>="2021-02-01 00:00:00")[1]]
Actual_yc_sevillenca_2021

# Season 2021_2022 ####
season_data_2022_30Dec<-data[iSeason_2022[[1]][1417:length_iSeason_2022],]

res_2022_30Dec <- PhenoFlex(temp=season_data_2022_30Dec$Temp,
                              times=c(1: length(season_data_2022_30Dec$Temp)),
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

seasontemps_2022_30Dec<-season_data_2022_30Dec
seasontemps_2022_30Dec[,"x"]<-res_2022_30Dec$x
seasontemps_2022_30Dec[,"y"]<-res_2022_30Dec$y
seasontemps_2022_30Dec[,"z"]<-res_2022_30Dec$z
seasontemps_2022_30Dec <-add_date(seasontemps_2022_30Dec)

Actual_yc_sevillenca_2022<-seasontemps_2022_30Dec$y[which(seasontemps_2022_30Dec$Date>="2022-02-08 00:00:00")[1]]
Actual_yc_sevillenca_2022

# Calibration of yc and zc ####
# Calibration of yc and zc according to the forcing output

# Improving the calibration process with different values of yc, zc
# All possible combinations
# Initialize empty vectors to store the combinations
yc_values <- vector("integer")
zc_values <- vector("integer")

# Generating combinations for vector1 (30 to 52, moving by half unit). yc values according to forcing test
# should not surpass the value of 52 cp.
for (i in seq(Actual_yc_sevillenca_2021, Actual_yc_sevillenca_2022,by = 0.1)) {
  yc_values <- c(yc_values, i)
}

# Generating combinations for vector2 (700 to 1200, moving by 5 units)
for (j in seq(700, 1200, by = 5)) {
  zc_values <- c(zc_values, j)
}

# Forcing season list January
season_list_forcing_30Dec <- list()

for (i in c(1:2)) {
  num_rows <- nrow(season_list_21_22[[i]])
  season_list_forcing_30Dec[[i]] <- season_list_21_22[[i]][1417:num_rows,]
}

# Run the loop to get the best combination of yc and zc. The choice of the best combination is based
# smallest mean absolute error between the two years.

lowest_mae <- Inf  # Initialize with a large value
calibration_sevillenca <- NULL

for (yc in yc_values) {
  for (zc in zc_values) {
    
    # Model parameters here
    Pheno_fit_recalib <- c(yc, zc, 7.992534e-01, 2.681513e+01, 3.240997e+03, 9.921809e+03, 6.867300e+03, 5.939913e+13, 7.492625e+00, 3.427934e+01, 2.959986e+00, 3.555938e+00)
    
    forc_Sevillenca <- df_Sevillenca[df_Sevillenca$Year %in% forcing_years, ]
    for (i in 1:nrow(forc_Sevillenca)) {
      num_rows <- nrow(season_list_forcing_30Dec[[i]])
      forc_Sevillenca[i, "Predicted"] <- PhenoFlex_GDHwrapper(season_list_forcing_30Dec[[i]], Pheno_fit_recalib)
    }
    
    Observed <- c(forc_Sevillenca[1,]$pheno,forc_Sevillenca[2,]$pheno)
    predicted <- c(forc_Sevillenca[1,]$Predicted, forc_Sevillenca[2,]$Predicted)
    mae <- mean(abs(Observed - predicted))
    
    # Update if the current combination has a lower MAE
    if (mae < lowest_mae) {
      lowest_mae <- mae
      calibration_sevillenca <- c(yc = yc, zc = zc, mae = lowest_mae)
    }
  }
}
print(calibration_sevillenca)

# Validation version 2 ####
# Re-calibrated parameters
Pheno_fit_sevillenca <- c(calibration_sevillenca[1], calibration_sevillenca[2], 7.992534e-01, 2.681513e+01, 3.240997e+03, 9.921809e+03, 6.867300e+03, 5.939913e+13, 7.492625e+00, 3.427934e+01, 2.959986e+00, 3.555938e+00)

# Validation data version 2
valid_df_Sevillenca_v2 <- df_Sevillenca[1:6,]

# Validation
for (i in 1 : 6) {
  valid_df_Sevillenca_v2[i, "Predicted"] <- PhenoFlex_GDHwrapper(valid_season_list_30Dec[[i]], Pheno_fit_sevillenca)
}

# Estimate the RMSEP
RMSEP_valid_v2_sevillenca <- RMSEP(valid_df_Sevillenca_v2$Predicted, valid_df_Sevillenca_v2$pheno, na.rm = TRUE)
RMSEP_valid_v2_sevillenca

# Compute the MAE
mae_valid_v2_sabatera <- mean(abs(valid_df_Sevillenca_v2$pheno - valid_df_Sevillenca_v2$Predicted), na.rm = TRUE)
mae_valid_v2_sabatera

##################
# Estimation of chill and heat using PhenoFlex and forcing test
# Fitting yc and zc to 8 years of bloom dates

# Initialize empty vectors to store the combinations
yc_values <- vector("integer")
zc_values <- vector("integer")

# Generating combinations for vector1
Actual_yc_sevillenca_2021
Actual_yc_sevillenca_2022

for (i in seq(Actual_yc_sevillenca_2021, Actual_yc_sevillenca_2022,by = 0.1)) {
  yc_values <- c(yc_values, i)
}

# Generating combinations for vector2 
for (j in seq(600, 1200, by = 5)) {
  zc_values <- c(zc_values, j)
}

# Then I generate a list of seasons 
calib_season_list_30Dec <- list()

for (i in c(1:8)) {
  num_rows <- nrow(calib_season_list[[i]])
  calib_season_list_30Dec[[i]] <- calib_season_list[[i]][1321:num_rows,]
}

# Run the loop to get the best combination of yc and zc. The choice of the best combination is based
# smallest RMSE.

lowest_RMSE <- Inf  # Initialize with a large value
chill_heat_sevillenca <- NULL

for (yc in yc_values) {
  for (zc in zc_values) {
    
    # Model parameters here
    Pheno_fit_recalib <- c(yc, zc, 7.992534e-01, 2.681513e+01, 3.240997e+03, 9.921809e+03, 6.867300e+03, 5.939913e+13, 7.492625e+00, 3.427934e+01, 2.959986e+00, 3.555938e+00)
    
    calib_Sevillenca <- df_Sevillenca
    for (i in 1:8) {
      calib_Sevillenca[i, "Predicted"] <- PhenoFlex_GDHwrapper(calib_season_list_30Dec[[i]], Pheno_fit_recalib)
    }
    
    RMSE <- RMSEP(calib_Sevillenca$Predicted, calib_Sevillenca$pheno, na.rm = TRUE)
    
    # Update if the current combination has a lower MAE
    if (RMSE < lowest_RMSE) {
      lowest_RMSE <- RMSE
      chill_heat_sevillenca <- c(yc = yc, zc = zc, RMSE = lowest_RMSE)
    }
  }
}
print(chill_heat_sevillenca)

# Application of the default model
calib_Sevillenca <- df_Sevillenca
for (i in 1:8) {
  calib_Sevillenca[i, "Predicted"] <- PhenoFlex_GDHwrapper(calib_season_list_30Dec[[i]], Pheno_fit$par)
}

RMSE_sevillenca_v0 <- RMSEP(calib_Sevillenca$Predicted, calib_Sevillenca$pheno, na.rm = TRUE)
RMSE_sevillenca_v0
