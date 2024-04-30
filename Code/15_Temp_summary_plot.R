# This script is to plot temperature data for 2021 and 2022 in Tassaout
library(readxl)
library(tidyverse)
library(chillR)

# Temperatures for the two years of forcing tests
Temp_2021_2022 <- read_excel("Max_Min_Temp_2021_2022.xlsx",col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric"))
Temp_2021_2022 <- as.data.frame(Temp_2021_2022)

Temp_2021_2022 <- add_date(Temp_2021_2022)
Temp_2021_2022$Year <- as.factor(Temp_2021_2022$Year)

Temp_2021_2022 <- Temp_2021_2022[-c(1:300),]
Temp_2021_2022 <- Temp_2021_2022 %>%
  pivot_longer(cols = c(Tmax, Tmin, Tmean),
               names_to = "Treatment",
               values_to = "Temperature") %>%
  select(Date, Year, Treatment, Temperature)

# Rolling mean of 11 days
library(zoo)
Sys.setlocale("LC_TIME", "English")

Temp_run_mean <- Temp_2021_2022 %>%
  group_by(Treatment) %>%
  arrange(Date) %>%
  mutate(RunningMean = rollmean(Temperature, k = 11, align = "center", fill = NA))

Temp_run_mean <- na.omit(Temp_run_mean)
writexl::write_xlsx(Temp_run_mean,"Temp_run_mean.xlsx")

Temp_run_mean <- read_excel("Temp_run_mean.xlsx")


# Create the plot using ggplot2
Temp_plot <- ggplot(Temp_run_mean, aes(x = Date, y = RunningMean, color = Treatment)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ Year,scales = "free_x") +
  labs(x = "Month", y = "Temperature (°C)", color = "Treatment") +
  theme_bw()+
  theme(legend.position = "top",
        axis.text = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold",size = 10),
        axis.title.y = element_text(face = "bold",size = 10)) +
  guides(color = guide_legend(title = NULL))
    
print(Temp_plot)    


ggsave("figures/Figure_S2.png", width = 22, height = 18, units = "cm", dpi = 600)







