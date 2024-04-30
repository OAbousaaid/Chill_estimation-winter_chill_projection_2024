# Forcing plots ####
# Needed libraries
library(tidyr)
library(tidyverse)
library(dplyr)
library(multcomp)
library(ggplot2)
library(scales)
library(ggpubr)
library(ggtext)
library(readxl)

# Set locale to English
Sys.setlocale("LC_TIME", "English")

# Leccino ####
# Upload data
forcing <- read_excel("Res_forc_vf.xlsx", sheet = "Feuil_vf",
                        col_types = c("date","text","text","text","numeric","numeric"))
forcing<- as.data.frame(forcing)

forcing_vf <- forcing[,c(1,3:6)]

forcing_Leccino <- forcing_vf %>%
  rename(Unforced=Poids_frais)%>%
  rename(Forced=Poids_frais_f)%>%
  pivot_longer(cols = c(Unforced, Forced),    
               names_to = "Treatment",        
               values_to = "Weight")%>%
  filter(Cultivar=="Leccino")

forcing_Leccino <- na.omit(forcing_Leccino)
forcing_Leccino$Date <- sub("2022", "2021", forcing_Leccino$Date)
forcing_Leccino$Date <- as.Date(forcing_Leccino$Date)


# Initial ggline plot
Leccino <- ggline(forcing_Leccino,x = "Date",y = "Weight", add = "mean_se",
                  color = "Treatment", facet.by = "Year") +
  
  stat_compare_means(aes(group = Treatment), label = "p.signif", paired = TRUE,
                     method = "t.test", vjust = 0.5) 

# Final plot
Leccino <- facet(Leccino, facet.by = "Year", scales = "free_x", panel.labs.font = list(face = "bold")) +
  labs(x = "", y = "") +
  scale_color_manual(values = c("green4", "red"),
                     name = "",
                     breaks = c("Unforced", "Forced"),
                     labels = c("**Unforced**", "**Forced**")) +
  ggtitle("Leccino") +
  theme(plot.title = element_text(color = "black", size = 14, face = "bold.italic", hjust = 0.5, vjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        legend.text = element_markdown(),
        axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d-%b",limits = as.Date(c("2021-01-01", "2021-04-01"))) +
  geom_vline(data = data.frame(xintercept = as.Date(c("2021-02-25", "2021-03-08")), 
                               Year = c("2021", "2022")),
             aes(xintercept = xintercept),
             linetype = "dotdash",
             color = "blue1",
             alpha = 0.5, lwd = 1, show.legend = FALSE) +
  scale_y_continuous(limits = c(0,107),breaks = seq(0, 107, by = 20))

# Display the plot
Leccino

# Sabatera ####
# Upload data
forcing_Sabatera <- forcing_vf %>%
  rename(Unforced=Poids_frais)%>%
  rename(Forced=Poids_frais_f)%>%
  pivot_longer(cols = c(Unforced, Forced),    
               names_to = "Treatment",        
               values_to = "Weight")%>%
  filter(Cultivar=="Sabatera")

forcing_Sabatera <- na.omit(forcing_Sabatera)
forcing_Sabatera$Date <- sub("2022", "2021", forcing_Sabatera$Date)
forcing_Sabatera$Date <- as.Date(forcing_Sabatera$Date)

# Visualization

# Initial ggline plot
Sabatera <- ggline(forcing_Sabatera, x = "Date", y = "Weight", add = "mean_se",
       color = "Treatment",facet.by = "Year")+
  stat_compare_means(aes(group = Treatment), label = "p.signif",paired = T,
                     method = "t.test",vjust = 0.5)

# Final plot
Sabatera <- facet(Sabatera,facet.by = "Year",scales = "free_x",panel.labs.font = list(face = "bold"))+
    labs(x="", y="")+
    scale_color_manual(values=c("green4", "red"),
                       name="",
                       breaks=c("Unforced", "Forced"),
                       labels=c("**Unforced**", "**Forced**"))+
    ggtitle("Sabatera")+
    theme(plot.title = element_text(color="black", size=14, face="bold.italic",
                                    hjust = 0.5,vjust = 0.5))+
    theme(axis.title.x = element_text(face="bold", size=10))+
    theme(axis.title.y = element_text(face="bold", size=10))+
    theme(legend.text = element_markdown(),
          axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d-%b",limits = as.Date(c("2021-01-01", "2021-04-01")))+
  geom_vline(data = data.frame(xintercept = as.Date(c("2021-02-20", "2021-02-01")), 
                               Year = c("2021", "2022")),
             aes(xintercept = xintercept),
             linetype = "dotdash",
             color = "blue1",
             alpha = 0.5, lwd = 1, show.legend = FALSE) +
    scale_y_continuous(limits = c(0,107),breaks = seq(0, 107, by = 20))

# Display the plot
Sabatera

# Sevillenca ####
# Upload data
forcing_Sevillenca <- forcing_vf %>%
  rename(Unforced=Poids_frais)%>%
  rename(Forced=Poids_frais_f)%>%
  pivot_longer(cols = c(Unforced, Forced),    
               names_to = "Treatment",        
               values_to = "Weight")%>%
  filter(Cultivar=="Sevillenca")

forcing_Sevillenca <- na.omit(forcing_Sevillenca)
forcing_Sevillenca$Date <- sub("2022", "2021", forcing_Sevillenca$Date)
forcing_Sevillenca$Date <- as.Date(forcing_Sevillenca$Date)

# Visualization

# Initial ggline plot
Sevillenca <- ggline(forcing_Sevillenca, x = "Date", y = "Weight", add = "mean_se",
       color = "Treatment",facet.by = "Year")+
  stat_compare_means(aes(group = Treatment), label = "p.signif",paired = T,
                     method = "t.test",vjust = 0.5)

# Final plot
Sevillenca <- facet(Sevillenca,facet.by = "Year",scales = "free_x",panel.labs.font = list(face = "bold"))+
    labs(x="", y="")+
    scale_color_manual(values=c("green4", "red"),
                       name="",
                       breaks=c("Unforced", "Forced"),
                       labels=c("**Unforced**", "**Forced**"))+
    ggtitle("Sevillenca")+
    theme(plot.title = element_text(color="black", size=14, face="bold.italic",
                                    hjust = 0.5,vjust = 0.5))+
    theme(axis.title.x = element_text(face="bold", size=12))+
    theme(axis.title.y = element_text(face="bold", size=12))+
    theme(legend.text = element_markdown(),
          axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d-%b",limits = as.Date(c("2021-01-01", "2021-04-01")))+
  geom_vline(data = data.frame(xintercept = as.Date(c("2021-02-01", "2021-02-08")), 
                               Year = c("2021", "2022")),
             aes(xintercept = xintercept),
             linetype = "dotdash",
             color = "blue1",
             alpha = 0.5, lwd = 1, show.legend = FALSE) +
    scale_y_continuous(limits = c(0,107),breaks = seq(0, 107, by = 20))

# Display the plot
Sevillenca

# Arbequina ####
# Upload data
forcing_Arbequina <- forcing_vf %>%
  rename(Unforced=Poids_frais)%>%
  rename(Forced=Poids_frais_f)%>%
  pivot_longer(cols = c(Unforced, Forced),    
               names_to = "Treatment",        
               values_to = "Weight")%>%
  filter(Cultivar=="Arbequina")

forcing_Arbequina <- na.omit(forcing_Arbequina)
forcing_Arbequina$Date <- sub("2022", "2021", forcing_Arbequina$Date)
forcing_Arbequina$Date <- as.Date(forcing_Arbequina$Date)

# Visualization

# Initial ggline plot
Arbequina <- ggline(forcing_Arbequina, x = "Date", y = "Weight", add = "mean_se",
       color = "Treatment",facet.by = "Year")+
  stat_compare_means(aes(group = Treatment), label = "p.signif",paired = T,
                     method = "t.test",vjust = 0.5)

# Final plot 
Arbequina <- facet(Arbequina,facet.by = "Year",scales = "free_x",panel.labs.font = list(face = "bold"))+
    labs(x="", y="")+
    scale_color_manual(values=c("green4", "red"),
                       name="",
                       breaks=c("Unforced", "Forced"),
                       labels=c("**Unforced**", "**Forced**"))+
    ggtitle("Arbequina")+
    theme(plot.title = element_text(color="black", size=14, face="bold.italic",
                                    hjust = 0.5,vjust = 0.5))+
    theme(axis.title.x = element_text(face="bold", size=12))+
    theme(axis.title.y = element_text(face="bold", size=12))+
    theme(legend.text = element_markdown(),
          axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d-%b",limits = as.Date(c("2021-01-01", "2021-04-01")))+
  geom_vline(data = data.frame(xintercept = as.Date(c("2021-02-20", "2021-02-22")), 
                               Year = c("2021", "2022")),
             aes(xintercept = xintercept),
             linetype = "dotdash",
             color = "blue1",
             alpha = 0.5, lwd = 1, show.legend = FALSE) +
    scale_y_continuous(limits = c(0,107),breaks = seq(0, 107, by = 20))
  
# Display the plot
Arbequina

# Frantoio ####
# Upload data
forcing_Frantoio <- forcing_vf %>%
  rename(Unforced=Poids_frais)%>%
  rename(Forced=Poids_frais_f)%>%
  pivot_longer(cols = c(Unforced, Forced),    
               names_to = "Treatment",        
               values_to = "Weight")%>%
  filter(Cultivar=="Frantoio")

forcing_Frantoio <- na.omit(forcing_Frantoio)
forcing_Frantoio$Date <- sub("2022", "2021", forcing_Frantoio$Date)
forcing_Frantoio$Date <- as.Date(forcing_Frantoio$Date)

# Visualization

# Initial ggline plot
Frantoio <- ggline(forcing_Frantoio, x = "Date", y = "Weight", add = "mean_se",
       color = "Treatment",facet.by = "Year")+
  stat_compare_means(aes(group = Treatment), label = "p.signif",paired = T,
                     method = "t.test",vjust = 0.5)

# Final plot
Frantoio <- facet(Frantoio,facet.by = "Year",scales = "free_x",panel.labs.font = list(face = "bold"))+
    labs(x="", y="")+
    scale_color_manual(values=c("green4", "red"),
                       name="",
                       breaks=c("Unforced", "Forced"),
                       labels=c("**Unforced**", "**Forced**"))+
    ggtitle("Frantoio")+
    theme(plot.title = element_text(color="black", size=14, face="bold.italic",
                                    hjust = 0.5,vjust = 0.5))+
    theme(axis.title.x = element_text(face="bold", size=12))+
    theme(axis.title.y = element_text(face="bold", size=12))+
    theme(legend.text = element_markdown(),
          axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d-%b",limits = as.Date(c("2021-01-01", "2021-04-01")))+
  geom_vline(data = data.frame(xintercept = as.Date(c("2021-02-20", "2021-03-01")), 
                               Year = c("2021", "2022")),
             aes(xintercept = xintercept),
             linetype = "dotdash",
             color = "blue1",
             alpha = 0.5, lwd = 1, show.legend = FALSE) +
    scale_y_continuous(limits = c(0,107),breaks = seq(0, 107, by = 20))

# Display the plot
Frantoio

# Beladi ####
# Upload data
forcing_Beladi <- forcing_vf %>%
  rename(Unforced=Poids_frais)%>%
  rename(Forced=Poids_frais_f)%>%
  pivot_longer(cols = c(Unforced, Forced),    
               names_to = "Treatment",        
               values_to = "Weight")%>%
  filter(Cultivar=="Beladi")

forcing_Beladi <- na.omit(forcing_Beladi)
forcing_Beladi$Date <- sub("2022", "2021", forcing_Beladi$Date)
forcing_Beladi$Date <- as.Date(forcing_Beladi$Date)

# Visualization
# Initial ggline plot
Beladi <- ggline(forcing_Beladi, x = "Date", y = "Weight", add = "mean_se",
       color = "Treatment",facet.by = "Year")+
  stat_compare_means(aes(group = Treatment), label = "p.signif",paired = T,
                     method = "t.test",vjust = 0.5)

# Final plot
Beladi <- facet(Beladi,facet.by = "Year",scales = "free_x",panel.labs.font = list(face = "bold"))+
    labs(x="", y="")+
    scale_color_manual(values=c("green4", "red"),
                       name="",
                       breaks=c("Unforced", "Forced"),
                       labels=c("**Unforced**", "**Forced**"))+
    ggtitle("Beladi")+
    theme(plot.title = element_text(color="black", size=14, face="bold.italic",
                                    hjust = 0.5,vjust = 0.5))+
    theme(axis.title.x = element_text(face="bold", size=12))+
    theme(axis.title.y = element_text(face="bold", size=12))+
    theme(legend.text = element_markdown(),
          axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d-%b",limits = as.Date(c("2021-01-01", "2021-04-01")))+
  geom_vline(data = data.frame(xintercept = as.Date(c("2021-01-20", "2021-02-08")), 
                               Year = c("2021", "2022")),
             aes(xintercept = xintercept),
             linetype = "dotdash",
             color = "blue1",
             alpha = 0.5, lwd = 1, show.legend = FALSE) +
    scale_y_continuous(limits = c(0,107),breaks = seq(0, 107, by = 20))

# Display the plot
Beladi

# Koroneiki ####
forcing_Koroneiki <- forcing_vf %>%
  rename(Unforced=Poids_frais)%>%
  rename(Forced=Poids_frais_f)%>%
  pivot_longer(cols = c(Unforced, Forced),    
               names_to = "Treatment",        
               values_to = "Weight")%>%
  filter(Cultivar=="Koroneiki")

forcing_Koroneiki <- na.omit(forcing_Koroneiki)
forcing_Koroneiki$Date <- sub("2022", "2021", forcing_Koroneiki$Date)
forcing_Koroneiki$Date <- as.Date(forcing_Koroneiki$Date)

# Visualization
# Initial ggline plot
Koroneiki <- ggline(forcing_Koroneiki, x = "Date", y = "Weight", add = "mean_se",
       color = "Treatment",facet.by = "Year")+
  stat_compare_means(aes(group = Treatment), label = "p.signif",paired = T,
                     method = "t.test",vjust = 0.5)

# Final plot
Koroneiki <- facet(Koroneiki,facet.by = "Year",scales = "free_x",panel.labs.font = list(face = "bold"))+
    labs(x="Sampling date", y="")+
    scale_color_manual(values=c("green4", "red"),
                       name="",
                       breaks=c("Unforced", "Forced"),
                       labels=c("**Unforced**", "**Forced**"))+
    ggtitle("Koroneiki")+
    theme(plot.title = element_text(color="black", size=14, face="bold.italic",
                                    hjust = 0.5,vjust = 0.5))+
    theme(axis.title.x = element_text(face="bold", size=12))+
    theme(axis.title.y = element_text(face="bold", size=12))+
    theme(legend.text = element_markdown(),
          axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d-%b",limits = as.Date(c("2021-01-01", "2021-04-01")))+
  geom_vline(data = data.frame(xintercept = as.Date(c("2021-02-25", "2021-02-22")), 
                               Year = c("2021", "2022")),
             aes(xintercept = xintercept),
             linetype = "dotdash",
             color = "blue1",
             alpha = 0.5, lwd = 1, show.legend = FALSE) +
    scale_y_continuous(limits = c(0,107),breaks = seq(0, 107, by = 20))
  
# Display the plot
Koroneiki

# Dressi ####
forcing_Dressi <- forcing_vf %>%
  rename(Unforced=Poids_frais)%>%
  rename(Forced=Poids_frais_f)%>%
  pivot_longer(cols = c(Unforced, Forced),    
               names_to = "Treatment",        
               values_to = "Weight")%>%
  filter(Cultivar=="Dressi")

forcing_Dressi <- na.omit(forcing_Dressi)
forcing_Dressi$Date <- sub("2022", "2021", forcing_Dressi$Date)
forcing_Dressi$Date <- as.Date(forcing_Dressi$Date)

# Visualization
# Initial ggline plot
Dressi <- ggline(forcing_Dressi, x = "Date", y = "Weight", add = "mean_se",
       color = "Treatment",facet.by = "Year")+
  stat_compare_means(aes(group = Treatment), label = "p.signif",paired = T,
                     method = "t.test",vjust = 0.5)

# Final plot
Dressi <- facet(Dressi,facet.by = "Year",scales = "free_x",panel.labs.font = list(face = "bold"))+
    labs(x="", y="")+
    scale_color_manual(values=c("green4", "red"),
                       name="",
                       breaks=c("Unforced", "Forced"),
                       labels=c("**Unforced**", "**Forced**"))+
    ggtitle("Dressi")+
    theme(plot.title = element_text(color="black", size=14, face="bold.italic",
                                    hjust = 0.5,vjust = 0.5))+
    theme(axis.title.x = element_text(face="bold", size=12))+
    theme(axis.title.y = element_text(face="bold", size=12))+
    theme(legend.text = element_markdown(),
          axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d-%b",limits = as.Date(c("2021-01-01", "2021-04-01")))+
  geom_vline(data = data.frame(xintercept = as.Date(c("2021-01-20", "2021-02-08")), 
                               Year = c("2021", "2022")),
             aes(xintercept = xintercept),
             linetype = "dotdash",
             color = "blue1",
             alpha = 0.5, lwd = 1, show.legend = FALSE) +
    scale_y_continuous(limits = c(0,107),breaks = seq(0, 107, by = 20))

# Display the plot
Dressi

# Grappolo ####
forcing_Grappolo <- forcing_vf %>%
  rename(Unforced=Poids_frais)%>%
  rename(Forced=Poids_frais_f)%>%
  pivot_longer(cols = c(Unforced, Forced),    
               names_to = "Treatment",        
               values_to = "Weight")%>%
  filter(Cultivar=="Grappolo")

forcing_Grappolo <- na.omit(forcing_Grappolo)
forcing_Grappolo$Date <- sub("2022", "2021", forcing_Grappolo$Date)
forcing_Grappolo$Date <- as.Date(forcing_Grappolo$Date)

# Visualization
# Intial ggline plot
Grappolo <- ggline(forcing_Grappolo, x = "Date", y = "Weight", add = "mean_se",
       color = "Treatment",facet.by = "Year")+
  stat_compare_means(aes(group = Treatment), label = "p.signif",paired = T,
                     method = "t.test",vjust = 0.5)

# Final plot
Grappolo <- facet(Grappolo,facet.by = "Year",scales = "free_x",panel.labs.font = list(face = "bold"))+
    labs(x="", y="Floral bud fresh weights (mg)")+
    scale_color_manual(values=c("green4", "red"),
                       name="",
                       breaks=c("Unforced", "Forced"),
                       labels=c("**Unforced**", "**Forced**"))+
    ggtitle("Grappolo")+
    theme(plot.title = element_text(color="black", size=14, face="bold.italic",
                                    hjust = 0.5,vjust = 0.5))+
    theme(axis.title.x = element_text(face="bold", size=12))+
    theme(axis.title.y = element_text(face="bold", size=12))+
    theme(legend.text = element_markdown(),
          axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d-%b",limits = as.Date(c("2021-01-01", "2021-04-01")))+
  geom_vline(data = data.frame(xintercept = as.Date(c("2021-02-11", "2021-02-22")), 
                               Year = c("2021", "2022")),
             aes(xintercept = xintercept),
             linetype = "dotdash",
             color = "blue1",
             alpha = 0.5, lwd = 1, show.legend = FALSE) +
    scale_y_continuous(limits = c(0,107),breaks = seq(0, 107, by = 20))

# Display the plot
Grappolo

# Plotting Figure S1 ####
library(patchwork)
(Sabatera|Dressi|Beladi)/
  (Grappolo|Arbequina|Sevillenca)/
  (Frantoio|Koroneiki|Leccino)+plot_layout(guides = "collect", 
                                           widths = c(0.1, widths)) & ggplot2::theme(legend.position = "bottom")

ggsave("figures/Figure_S1.png", width = 40, height = 28, units = "cm", dpi = 600)
