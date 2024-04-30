# Leccino ####

# Forcing Plot ####
forcing_Leccino <- forcing_vf %>%
  rename(Unforced=Poids_frais)%>%
  rename(Forced=Poids_frais_f)%>%
  pivot_longer(cols = c(Unforced, Forced),    
               names_to = "Treatment",        
               values_to = "Weight")%>%
  filter(Cultivar=="Leccino")

forcing_Leccino <- na.omit(forcing_Leccino)
forcing_Leccino$Date <- as.Date(forcing_Leccino$Date)

# Visualization
# The Plot
Leccino <- ggline(forcing_Leccino, x = "Date", y = "Weight", add = "mean_se",
                  color = "Treatment",facet.by = "Year")+
  stat_compare_means(aes(group = Treatment), label = "p.signif",paired = T,
                     method = "t.test",vjust = 0.5)

# X axis start date and break
breaks <- seq(as.Date("2021-01-01"), as.Date("2022-12-31"), by = "17 days")

Leccino <- facet(Leccino,facet.by = "Year",scales = "free",panel.labs.font = list(face = "bold"))+
  labs(x="Sampling date", y="Floral bud fresh weights (mg)")+
  scale_color_manual(values=c("cornflowerblue", "coral"),
                     name="",
                     breaks=c("Unforced", "Forced"),
                     labels=c("**Unforced**", "**Forced**"))+
  ggtitle("Leccino")+
  theme(plot.title = element_text(color="black", size=14, face="bold.italic",
                                  hjust = 0.5,vjust = 0.5))+
  theme(axis.title.x = element_text(face="bold", size=12))+
  theme(axis.title.y = element_text(face="bold", size=12))+
  theme(legend.text = element_markdown())+
  scale_x_date(breaks = breaks, date_labels = "%b %d")


Leccino <- Leccino + geom_vline(xintercept = as.Date(c("2021-02-25", "2022-03-08")), 
                                linetype = "dotdash", color = "green4",alpha=0.5,lwd=1)
Leccino

# Validation version 1 ####

# Validation of the fitted PM PhenoFlex
# Validation Version 1; from the first level of calibration without forcing
# Set the validation seasons
validation_seasons <- df_Leccino[1:6,1]
forcing_years <- df_Leccino[8:9,1]

# Then I generate a list of seasons with weather data for the validation procedure
valid_season_list <- genSeasonList(data, mrange = c(11, 6), years = validation_seasons)

# Season list selection starting from the season onset set by PLS out put (Abou-Saaid & al 2022)
as.POSIXlt(c("21.11"), format = "%d.%m")$yday+1
as.Date(325, format="%d.%m",origin = "01.01")-1

valid_season_list_leccino <- list()

for (i in c(1:6)) {
  num_rows <- nrow(valid_season_list[[i]])
  valid_season_list_leccino[[i]] <- valid_season_list[[i]][481:num_rows,]
}

# Upload fitted Olive PhenoFlex model
Pheno_fit <- readRDS("PhenFlex_olive.rds")

# Validation data version 1
valid_df_leccino_v1 <- df_Leccino[1:6,]

# Validation
for (i in 1 : 6) {
  
  valid_df_leccino_v1[i, "Predicted"] <- PhenoFlex_GDHwrapper(valid_season_list_leccino[[i]], Pheno_fit$par)
}

# Estimate the RMSEP
RMSEP_valid_v1_leccino <- RMSEP(valid_df_leccino_v1$Predicted, valid_df_leccino_v1$pheno, na.rm = TRUE)
RMSEP_valid_v1_leccino

# Compute the MAE
mae_valid_v1_leccino <- mean(abs(valid_df_leccino_v1$pheno - valid_df_leccino_v1$Predicted), na.rm = TRUE)
mae_valid_v1_leccino

# Observed 'yc' and 'zc' ####
# To visualize the actual yc and zc starting from the season onset given by PLS (Abou-Saaid et., 2022)
as.POSIXlt(c("21.11"), format = "%d.%m")$yday+1
as.Date(325, format="%d.%m",origin = "01.01")-1

# Season 2020_2021 ####
iSeason_2021 <- genSeason(data,
                          mrange = c(11, 6),
                          years=c(2021))

length_iSeason_2021 <- length(iSeason_2021[[1]])

season_data_2021_leccino<-data[iSeason_2021[[1]][481:length_iSeason_2021],]

# Fitted yc and zc of PM
yc=5.800000e+01
zc=7.850000e+02

res_2021_leccino <- PhenoFlex(temp=season_data_2021_leccino$Temp,
                      times=c(1: length(season_data_2021_leccino$Temp)),
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

seasontemps_2021_leccino<-season_data_2021_leccino
seasontemps_2021_leccino[,"x"]<-res_2021_leccino$x
seasontemps_2021_leccino[,"y"]<-res_2021_leccino$y
seasontemps_2021_leccino[,"z"]<-res_2021_leccino$z
seasontemps_2021_leccino<-add_date(seasontemps_2021_leccino)

Actual_yc_leccino_2021<-seasontemps_2021_leccino$y[which(seasontemps_2021_leccino$Date>="2021-02-25 00:00:00")[1]]
Actual_yc_leccino_2021

# Season 2021_2022 ####
iSeason_2022 <- genSeason(data,
                          mrange = c(11, 6),
                          years=c(2022))

length_iSeason_2022 <- length(iSeason_2022[[1]])

season_data_2022_leccino<-data[iSeason_2022[[1]][481:length_iSeason_2022],]

res_2022_leccino <- PhenoFlex(temp=season_data_2022_leccino$Temp,
                      times=c(1: length(season_data_2022_leccino$Temp)),
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

seasontemps_2022_leccino<-season_data_2022_leccino
seasontemps_2022_leccino[,"x"]<-res_2022_leccino$x
seasontemps_2022_leccino[,"y"]<-res_2022_leccino$y
seasontemps_2022_leccino[,"z"]<-res_2022_leccino$z
seasontemps_2022_leccino <-add_date(seasontemps_2022_leccino)

Actual_yc_leccino_2022<-seasontemps_2022_leccino$y[which(seasontemps_2022_leccino$Date>="2022-03-08 00:00:00")[1]]
Actual_yc_leccino_2022

# Calibration of yc and zc ####
# Calibration of yc and zc according to the forcing output

# Improving the calibration process with different values of yc, zc
# All possible combinations
# Initialize empty vectors to store the combinations
yc_values <- vector("integer")
zc_values <- vector("integer")

# Generating combinations for vector1 (30 to 60, moving by half unit). yc values according to forcing tets
# should not surpass the value of 60 cp.
for (i in seq(Actual_yc_leccino_2021,Actual_yc_leccino_2022,by = 0.1)) {
  yc_values <- c(yc_values, i)
}

# Generating combinations for vector2 (400 to 900, moving by 5 units)
for (j in seq(700, 1200, by = 5)) {
  zc_values <- c(zc_values, j)
}

# Then I generate a list of seasons with weather data for the forcing calibration procedure (2021 and 2022)
season_list_21_22 <- genSeasonList(data, mrange = c(11, 6), years = forcing_years)

season_list_forcing_leccino <- list()

for (i in c(1:2)) {
  num_rows <- nrow(season_list_21_22[[i]])
  season_list_forcing_leccino[[i]] <- season_list_21_22[[i]][481:num_rows,]
}

# Run the loop to get the best combination of yc and zc. The choice of the best combination is based
# smallest mean absolute error between the two years.

lowest_mae <- Inf  # Initialize with a large value
calibration_leccino <- NULL

for (yc in yc_values) {
  for (zc in zc_values) {
    
    # Model parameters here
    Pheno_fit_recalib <- c(yc, zc, 7.992534e-01, 2.681513e+01, 3.240997e+03, 9.921809e+03, 6.867300e+03, 5.939913e+13, 7.492625e+00, 3.427934e+01, 2.959986e+00, 3.555938e+00)
    
    forc_Leccino <- df_Leccino[df_Leccino$Year %in% forcing_years, ]
    for (i in 1:nrow(forc_Leccino)) {
      num_rows <- nrow(season_list_forcing_leccino[[i]])
      forc_Leccino[i, "Predicted"] <- PhenoFlex_GDHwrapper(season_list_forcing_leccino[[i]], Pheno_fit_recalib)
    }
    
    Observed <- c(forc_Leccino[1,]$pheno,forc_Leccino[2,]$pheno)
    predicted <- c(forc_Leccino[1,]$Predicted, forc_Leccino[2,]$Predicted)
    mae <- mean(abs(Observed - predicted))
    
    # Update if the current combination has a lower MAE
    if (mae < lowest_mae) {
      lowest_mae <- mae
      calibration_leccino <- c(yc = yc, zc = zc, mae = lowest_mae)
    }
  }
}
print(calibration_leccino)

# Validation version 2 ####
# Re-calibrated parameters
Pheno_fit_leccino <- c(calibration_leccino[1], calibration_leccino[2], 7.992534e-01, 2.681513e+01, 3.240997e+03, 9.921809e+03, 6.867300e+03, 5.939913e+13, 7.492625e+00, 3.427934e+01, 2.959986e+00, 3.555938e+00)

# Validation data version 2
valid_df_leccino_v2 <- df_Leccino[1:6,]

# Validation
for (i in 1 : 6) {
  valid_df_leccino_v2[i, "Predicted"] <- PhenoFlex_GDHwrapper(valid_season_list_leccino[[i]], Pheno_fit_leccino)
}

# Estimate the RMSEP
RMSEP_valid_v2_leccino <- RMSEP(valid_df_leccino_v2$Predicted, valid_df_leccino_v2$pheno, na.rm = TRUE)
RMSEP_valid_v2_leccino

# Compute the MAE
mae_valid_v2_leccino <- mean(abs(valid_df_leccino_v2$pheno - valid_df_leccino_v2$Predicted), na.rm = TRUE)
mae_valid_v2_leccino

# Comment
# We can see that the observed endodormancy dates for 2021 and 2022 can improve the model
# performance, from a RMSE of 14 days with the amounts of chill and heat fitted for PM to
# a RSME of 6.9 with the fitted 'yc' and 'zc' according to forcing experiments during 2021
# and 2022. This validates the thermal requirements of the cultivar Leccino.

##################
# Estimation of chill and heat using PhenoFlex and forcing test
# Fitting yc and zc to 8 years of bloom dates

# Initialize empty vectors to store the combinations
yc_values <- vector("integer")
zc_values <- vector("integer")

# Generating combinations for vector1 (30 to 60, moving by half unit). yc values according to forcing tets
# should not surpass the value of 60 cp.
for (i in seq(Actual_yc_leccino_2021, Actual_yc_leccino_2022,by = 0.1)) {
  yc_values <- c(yc_values, i)
}

# Generating combinations for vector2 (400 to 900, moving by 5 units)
for (j in seq(600, 1200, by = 5)) {
  zc_values <- c(zc_values, j)
}

# Then I generate a list of seasons with weather data for the forcing calibration procedure (2021 and 2022)
calibration_years <- df_Leccino[1:8,1]
calib_season_list <- genSeasonList(data, mrange = c(11, 6), years = calibration_years)

calib_season_list_21Nov <- list()

for (i in c(1:8)) {
  num_rows <- nrow(calib_season_list[[i]])
  calib_season_list_21Nov[[i]] <- calib_season_list[[i]][481:num_rows,]
}

# Run the loop to get the best combination of yc and zc. The choice of the best combination is based
# smallest mean absolute error between the two years.

lowest_RMSE <- Inf  # Initialize with a large value
chill_heat_leccino <- NULL

for (yc in yc_values) {
  for (zc in zc_values) {
    
    # Model parameters here
    Pheno_fit_recalib <- c(yc, zc, 7.992534e-01, 2.681513e+01, 3.240997e+03, 9.921809e+03, 6.867300e+03, 5.939913e+13, 7.492625e+00, 3.427934e+01, 2.959986e+00, 3.555938e+00)
    
    calib_Leccino <- df_Leccino
    for (i in 1:8) {
      calib_Leccino[i, "Predicted"] <- PhenoFlex_GDHwrapper(calib_season_list_21Nov[[i]], Pheno_fit_recalib)
    }
    
    RMSE <- RMSEP(calib_Leccino$Predicted, calib_Leccino$pheno, na.rm = TRUE)
    
    # Update if the current combination has a lower MAE
    if (RMSE < lowest_RMSE) {
      lowest_RMSE <- RMSE
      chill_heat_leccino <- c(yc = yc, zc = zc, RMSE = lowest_RMSE)
    }
  }
}
print(chill_heat_leccino)

# Default model
calib_Leccino <- df_Leccino
for (i in 1:8) {
  calib_Leccino[i, "Predicted"] <- PhenoFlex_GDHwrapper(calib_season_list_21Nov[[i]], Pheno_fit$par)
}

RMSE_leccino_v0 <- RMSEP(calib_Leccino$Predicted, calib_Leccino$pheno, na.rm = TRUE)
RMSE_leccino_v0

