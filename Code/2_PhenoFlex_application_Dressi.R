# Dressi ####

# Forcing plot ####
forcing_Dressi <- forcing_vf %>%
  rename(Unforced=Poids_frais)%>%
  rename(Forced=Poids_frais_f)%>%
  pivot_longer(cols = c(Unforced, Forced),    
               names_to = "Treatment",        
               values_to = "Weight")%>%
  filter(Cultivar=="Dressi")

forcing_Dressi <- na.omit(forcing_Dressi)
forcing_Dressi$Date <- as.Date(forcing_Dressi$Date)

# Visualization

# The Plot
Dressi <- ggline(forcing_Dressi, x = "Date", y = "Weight", add = "mean_se",
       color = "Treatment",facet.by = "Year")+
  stat_compare_means(aes(group = Treatment), label = "p.signif",paired = T,
                     method = "t.test",vjust = 0.5)

Dressi <- facet(Dressi,facet.by = "Year",scales = "free",panel.labs.font = list(face = "bold"))+
    labs(x="Sampling date", y="Floral bud fresh weights (mg)")+
    scale_color_manual(values=c("cornflowerblue", "coral"),
                       name="",
                       breaks=c("Unforced", "Forced"),
                       labels=c("**Unforced**", "**Forced**"))+
    ggtitle("Dressi")+
    theme(plot.title = element_text(color="black", size=14, face="bold.italic",
                                    hjust = 0.5,vjust = 0.5))+
    theme(axis.title.x = element_text(face="bold", size=12))+
    theme(axis.title.y = element_text(face="bold", size=12))+
    theme(legend.text = element_markdown())+
  scale_x_date(breaks = breaks, date_labels = "%b %d")


Dressi <- Dressi + geom_vline(xintercept = as.Date(c("2021-01-20", "2022-02-08")), 
                linetype = "dotdash", color = "green4",alpha=0.5,lwd=1)
Dressi

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
valid_df_dressi_v1 <- df_Dressi[1:6,]

# Validation
for (i in 1 : 6) {
  valid_df_dressi_v1[i, "Predicted"] <- PhenoFlex_GDHwrapper(valid_season_list_January[[i]], Pheno_fit$par)
}

# Estimate the RMSEP
RMSEP_valid_v1_dressi <- RMSEP(valid_df_dressi_v1$Predicted, valid_df_dressi_v1$pheno, na.rm = TRUE)
RMSEP_valid_v1_dressi

# Compute the MAE
mae_valid_v1_dressi <- mean(abs(valid_df_dressi_v1$pheno - valid_df_dressi_v1$Predicted), na.rm = TRUE)
mae_valid_v1_dressi

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

Actual_yc_dressi_2021<-seasontemps_2021_January$y[which(seasontemps_2021_January$Date>="2021-01-20 00:00:00")[1]]
Actual_yc_dressi_2021

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

Actual_yc_dressi_2022<-seasontemps_2022_January$y[which(seasontemps_2022_January$Date>="2022-02-08 00:00:00")[1]]
Actual_yc_dressi_2022

# Calibration of yc and zc ####
# Calibration of yc and zc according to the forcing output

# Improving the calibration process with different values of yc, zc

# Initialize empty vectors to store the combinations
yc_values <- vector("integer")
zc_values <- vector("integer")

# Generating combinations for vector1, yc values according to forcing test
for (i in seq(Actual_yc_dressi_2021,Actual_yc_dressi_2022,by = 0.1)) {
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
calibration_dressi <- NULL

for (yc in yc_values) {
  for (zc in zc_values) {
    
    # Model parameters here
    Pheno_fit_recalib <- c(yc, zc, 7.992534e-01, 2.681513e+01, 3.240997e+03, 9.921809e+03, 6.867300e+03, 5.939913e+13, 7.492625e+00, 3.427934e+01, 2.959986e+00, 3.555938e+00)
    
    forc_Dressi <- df_Dressi[df_Dressi$Year %in% forcing_years, ]
    for (i in 1:nrow(forc_Dressi)) {
      num_rows <- nrow(season_list_forcing_January[[i]])
      forc_Dressi[i, "Predicted"] <- PhenoFlex_GDHwrapper(season_list_forcing_January[[i]], Pheno_fit_recalib)
    }
    
    Observed <- c(forc_Dressi[1,]$pheno,forc_Dressi[2,]$pheno)
    predicted <- c(forc_Dressi[1,]$Predicted, forc_Dressi[2,]$Predicted)
    mae <- mean(abs(Observed - predicted))
    
    # Update if the current combination has a lower MAE
    if (mae < lowest_mae) {
      lowest_mae <- mae
      calibration_dressi <- c(yc = yc, zc = zc, mae = lowest_mae)
    }
  }
}
print(calibration_dressi)

# Validation version 2 ####
# Re-calibrated parameters
Pheno_fit_dressi <- c(calibration_dressi[1], calibration_dressi[2], 7.992534e-01, 2.681513e+01, 3.240997e+03, 9.921809e+03, 6.867300e+03, 5.939913e+13, 7.492625e+00, 3.427934e+01, 2.959986e+00, 3.555938e+00)

# Validation data version 2
valid_df_Dressi_v2 <- df_Dressi[1:6,]

# Validation
for (i in 1 : 6) {
  valid_df_Dressi_v2[i, "Predicted"] <- PhenoFlex_GDHwrapper(valid_season_list_January[[i]], Pheno_fit_dressi)
}

# Estimate the RMSEP
RMSEP_valid_v2_dressi <- RMSEP(valid_df_Dressi_v2$Predicted, valid_df_Dressi_v2$pheno, na.rm = TRUE)
RMSEP_valid_v2_dressi

# Compute the MAE
mae_valid_v2_dressi <- mean(abs(valid_df_Dressi_v2$pheno - valid_df_Dressi_v2$Predicted), na.rm = TRUE)
mae_valid_v2_dressi

##################
# Estimation of chill and heat using PhenoFlex and forcing test
# Fitting yc and zc to 8 years of bloom dates

# Initialize empty vectors to store the combinations
yc_values <- vector("integer")
zc_values <- vector("integer")

# Generating combinations for vector1
Actual_yc_dressi_2021
Actual_yc_dressi_2022

for (i in seq(Actual_yc_dressi_2021,Actual_yc_dressi_2022,by = 0.1)) {
  yc_values <- c(yc_values, i)
}

# Generating combinations for vector2 
for (j in seq(600, 1200, by = 5)) {
  zc_values <- c(zc_values, j)
}

# Run the loop to get the best combination of yc and zc. The choice of the best combination is based
# smallest RMSE.

lowest_RMSE <- Inf  # Initialize with a large value
chill_heat_dressi <- NULL

for (yc in yc_values) {
  for (zc in zc_values) {
    
    # Model parameters here
    Pheno_fit_recalib <- c(yc, zc, 7.992534e-01, 2.681513e+01, 3.240997e+03, 9.921809e+03, 6.867300e+03, 5.939913e+13, 7.492625e+00, 3.427934e+01, 2.959986e+00, 3.555938e+00)
    
    calib_Dressi <- df_Dressi
    for (i in 1:8) {
      calib_Dressi[i, "Predicted"] <- PhenoFlex_GDHwrapper(calib_season_list_January[[i]], Pheno_fit_recalib)
    }
    
    RMSE <- RMSEP(calib_Dressi$Predicted, calib_Dressi$pheno, na.rm = TRUE)
    
    # Update if the current combination has a lower MAE
    if (RMSE < lowest_RMSE) {
      lowest_RMSE <- RMSE
      chill_heat_dressi <- c(yc = yc, zc = zc, RMSE = lowest_RMSE)
    }
  }
}
print(chill_heat_dressi)

# Default model
calib_Dressi <- df_Dressi
for (i in 1:8) {
  calib_Dressi[i, "Predicted"] <- PhenoFlex_GDHwrapper(calib_season_list_January[[i]], Pheno_fit$par)
}

RMSE_dressi_v0 <- RMSEP(calib_Dressi$Predicted, calib_Dressi$pheno, na.rm = TRUE)
RMSE_dressi_v0
