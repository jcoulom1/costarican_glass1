#####
##Begin Costa Rica Glass project
#####

##Load data and packages
cr_data <- read.csv("C:/Users/labry/Documents/R/costarican_glass1/data/wt%_probe_data.csv")
library(dbplyr)
library(tidyverse)
library(gridExtra)
library(data.table)
library(huxtable)
library(kableExtra)


#create new column to sort by bulk rocks into a column for rockname

rock_cr1a <- cr_data %>%
   filter(grepl("CR1A", cr_data$Comment)) %>%  ##find all obs w/ CR1A
  mutate("RockName" = "CR1A")  ##create new col with rock name (CR1A)
rock_cr1b <- cr_data %>%
  filter(grepl("CR1B", cr_data$Comment)) %>%
  mutate("RockName" = "CR1B")
rock_cr2a <- cr_data %>%
  filter(grepl("CR2A", cr_data$Comment)) %>%
  mutate("RockName" = "CR2A")
rock_cr2b <- cr_data %>%
  filter(grepl("CR2B", cr_data$Comment)) %>%
  mutate("RockName" = "CR2B")
rock_cr3 <- cr_data %>%
  filter(grepl("CR3", cr_data$Comment)) %>%
  mutate("RockName" = "CR3")
rock_cr4 <- cr_data %>%
  filter(grepl("CR4", cr_data$Comment)) %>%
  mutate("RockName" = "CR4")
rock_cr5 <- cr_data %>%
  filter(grepl("CR5", cr_data$Comment)) %>%
  mutate("RockName" = "CR5")
rock_cr6 <- cr_data %>%
  filter(grepl("CR6", cr_data$Comment)) %>%
  mutate("RockName" = "CR6")
rock_cr7 <- cr_data %>%
  filter(grepl("CR7", cr_data$Comment)) %>%
  mutate("RockName" = "CR7")
rock_data <- rbind(rock_cr1a, rock_cr1b, rock_cr2a, rock_cr2b, rock_cr3,
                   rock_cr4, rock_cr5, rock_cr6, rock_cr7) #pull all sheets together
#to create one sheet with new column for all the rock names
rock_data <- rock_data[,c(30, 24, 2:16)] #reorder columns keeping what's needed
rock_data <- rock_data %>%
  mutate("MgN" = ((MgO / (40.31)) / ((MgO / (40.31)) + (FeO / (71.85)))) * (100)) ##create col for Mg#
rock_data$RockName <- as.factor(rock_data$RockName)  #chg Rockname col to factor

#filter for data constraints
rock_data[rock_data == ""] <- NA ##enters NA for all empty spots

rock_data_wt <- rock_data %>%
  select("RockName":"Total", "MgN") %>%  #choose relevant columns
  filter(Total > 95.0 & Total < 101.0) %>% #select rows based on Total
  filter(is.na(V2O3) & SiO2 < 90.0 & SiO2 > 40 & Al2O3 < 22.0 & Al2O3 > 10.0 & K2O > 1.0) #select rows based on elements



#Start to work on plots - SiO2 vs Na2O + K2O for all rocks
alkali_plot <- ggplot(rock_data_wt, aes(x = SiO2, y = Na2O + K2O)) +
  facet_wrap(vars(RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  labs(title = "Silice vs Alkali", x = "SiO2, Wt%", y = "Na2O + K2O, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3", 
                                "royalblue4", "firebrick3", "cyan3")) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
alkali_plot

##plot si vs mg
mg_plot_all <- ggplot(rock_data_wt, aes(x = SiO2, y = MgN)) +
  facet_wrap(vars(RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  labs(title = "Silica vs Magnesium Number", x = "SiO2, Wt%", y = "Mg #") + 
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3")) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
mg_plot_all


# plot si vs fe
fe_plot_all <- ggplot(rock_data_wt, aes(x = SiO2, y = FeO)) +
  facet_wrap(vars(RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  labs(title = "Silica vs Iron", x = "SiO2, Wt%", y = "FeO*, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3")) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
fe_plot_all

# plot si vs ca
ca_plot_all <- ggplot(rock_data_wt, aes(x = SiO2, y = CaO)) +
  facet_wrap(vars(RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  labs(title = "Silica vs Calcium", x = "SiO2, Wt%", y = "CaO, Wt%") + 
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3")) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
ca_plot_all


#plot si vs al
al_plot_all <- ggplot(rock_data_wt, aes(x = SiO2, y = Al2O3)) +
  facet_wrap(vars(RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  labs(title = "Silica vs Aluminum", x = "SiO2, Wt%", y = "Al2O3, Wt%") + 
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3")) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
al_plot_all


# plot al vs ti
alti_plot_all <- ggplot(rock_data_wt, aes(x = Al2O3, y = TiO2)) +
  facet_wrap(vars(RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  labs(title = "Aluminum vs Titanium", x = "Al2O3 Wt%", y = "TiO2 Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3")) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
alti_plot_all

#create TAS plot to create background for data layer
#create blank dataframe for tas plot
d = data.frame(x = c(40, 80), y = c(0,15))


theme_set(theme_bw(base_size=28))

#makes the TAS template
p <- ggplot(data=d, mapping=aes(x=x, y=y)) +
  geom_blank() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(limits=c(0,15), expand = c(0, 0)) + 
  scale_x_continuous(limits=c(40,80), expand = c(0, 0)) +
  labs(y=expression(Na[2]*O + K[2]*O*~ wt~'%'), x=expression(SiO[2]*~ wt~'%'))+
  annotate("segment", x=45, xend=45, y=1, yend=5)+
  annotate("segment", x=45, xend=52, y=5, yend=5)+
  annotate("segment", x=52, xend=69, y=5, yend=8)+
  annotate("segment", x=76.5, xend=69, y=1, yend=8)+
  annotate("segment", x=69, xend=69, y=8, yend=13)+
  annotate("segment", x=45, xend=61.32, y=5, yend=13.7)+
  annotate("segment", x=52, xend=52, y=1, yend=5)+
  annotate("segment", x=57, xend=57, y=1, yend=5.9)+
  annotate("segment", x=63, xend=63, y=1, yend=6.9)+
  annotate("segment", x=52, xend=49.4, y=5, yend=7.3)+
  annotate("segment", x=57, xend=53.05, y=5.9, yend=9.25)+
  annotate("segment", x=63, xend=57.6, y=6.9, yend=11.7)+
  annotate("segment", x=41, xend=45, y=3, yend=3)+
  annotate("segment", x=41, xend=41, y=1, yend=3)+
  annotate("segment", x=41, xend=41, y=3, yend=7, linetype="dashed")+
  annotate("segment", x=41, xend=45, y=7, yend=9.4, linetype="dashed")+
  annotate("segment", x=45, xend=52.5, y=9.4, yend=14)+
  annotate("segment", x=49.4, xend=45, y=7.3, yend=9.4)+
  annotate("segment", x=53, xend=48.4, y=9.3, yend=11.5)+
  annotate("segment", x=57.6, xend=50.3, y=11.7, yend=15)

tas <- p + annotate("text", label = "Basalt", x = 48.5, y = 2, size=4)+
  annotate("text", label = "Basaltic\n andesite", x = 54.3, y = 3.0, size=4)+
  annotate("text", label = "Andesite", x = 60, y = 3.5, size=4)+
  annotate("text", label = "Dacite", x = 67.5, y = 4.2, size=4)+
  annotate("text", label = "Rhyolite", x = 75, y = 7, size=4)+
  annotate("text", label = "Trachy- \n basalt", x = 48.8, y = 5.7, size=4)+
  annotate("text", label = "Basaltic \n trachy- \n andesite", x = 53, y = 7, size=4)+
  annotate("text", label = "Trachy- \n andesite", x = 57.8, y = 8.2, size=4)+
  annotate("text", label = "Trachydacite", x = 65, y = 9, size=4)+
  annotate("text", label = "Trachyte", x = 62.5, y = 11.5, size=4)+
  annotate("text", label = "Picro- \n basalt", x = 43, y = 1.5, size=4)+
  annotate("text", label = "Basanite \n (Ol > 10%)", x = 43.7, y = 6, size=4)+
  annotate("text", label = "Tephrite \n (Ol < 10%)", x = 45, y = 7.8, size=4)+
  annotate("text", label = "Phono- \n tephrite", x = 48.5, y = 9.5, size=4)+
  annotate("text", label = "Tephri- \n phonolite", x = 52.5, y = 11.5, size=4)+
  annotate("text", label = "Phonolite", x = 57, y = 14, size=4)+
  annotate("text", label = "Foidite", x = 45, y = 12, size=4)
tas


#plot alkali by rockname - all points
rock_alkplot <-  rock_data_wt %>%
  ggplot(mapping = aes(x = SiO2, y = Na2O + K2O, colour = RockName, legend(cex = 0.75))) +
  geom_point(aes(shape = RockName, color = RockName, size = 2)) +
  labs(title = "Alkali by Rock Name", x = "SiO2, Wt%", y = "Na2O + K2O, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3")) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
rock_alkplot


# use rock_data to add points to the tas diagram
tas +
  geom_point(data = rock_data_wt, aes(x = SiO2, y = Na2O + K2O, shape = RockName, color = RockName)) +
  labs(title = "Rocks Plotted on TAS Diagram", x = "SiO2, Wt%", y = "Na2O + K2O, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))



## show all rocks on one plot
#plot sio2 vs mg#
mg_plot <- rock_data %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, MgN, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Silica vs Magnesium", x = "SiO2, Wt%", y = "Mg #") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3"))
mg_plot <- mg_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) + 
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
mg_plot + scale_x_continuous(limits = c(50, 80))


#plot si vs fe all
fe_plot <- rock_data_wt %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, FeO, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Silica vs Iron", x = "SiO2, Wt%", y = "FeO*, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3"))
fe_plot <- fe_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
fe_plot + scale_x_continuous(limits = c(50, 80))


#plot si vs ca
ca_plot <- rock_data_wt %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, CaO, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Silica vs Calcium", x = "SiO2, Wt%", y = "CaO, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3"))
ca_plot <- ca_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) + 
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
ca_plot + scale_x_continuous(limits = c(50, 80))


#plot sio2 vs tio2
ti_plot <- rock_data %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, TiO2, color = RockName)) +
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Silica vs Aluminum", x = "SiO2, Wt%", y = "TiO2, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3"))
ti_plot <- ti_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) + 
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
ti_plot + scale_x_continuous(limits = c(50, 80))


#plot sio2 vs al
al_plot <- rock_data %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, Al2O3, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Silica vs Aluminum", x = "SiO2, Wt%", y = "Al2O3, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3"))
al_plot <- al_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) + 
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
al_plot + scale_x_continuous(limits = c(50, 80))


# plot al vs ti all
alti_plot <- rock_data_wt %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(Al2O3, TiO2, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Aluminum vs Titanium", x = "Al2O3, Wt%", y = "TiO2, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3"))
alti_plot <- alti_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
alti_plot

#create table for averages per rock
gls_avg <- rock_data_wt %>% #first create new table containing avg's
  group_by(RockName) %>%
  summarise("SiO2" = mean(SiO2), "TiO2" = mean(TiO2),
            "Al2O3" = mean(Al2O3), "Cr" = mean(Cr2O3),
            "MgO" = mean(MgO), "CaO" = mean(CaO), "MnO" = mean(MnO),
            "FeO" = mean(FeO), "Na2O" = mean(Na2O), "K2O" = mean(K2O),
            "S" = mean(S), "P2O5" = mean(P2O5), "MgN" = mean(MgN),
            "Total" = mean(Total))

## plot averages of all glass per rock

#plot alkali by rockname - all points
alk_avg_plot <-  gls_avg %>%
  ggplot(mapping = aes(x = SiO2, y = Na2O + K2O, colour = RockName, legend(cex = 0.75))) +
  geom_point(aes(shape = RockName, color = RockName, size = 2)) +
  labs(title = "Alkali Average by Rock Name", x = "SiO2, Wt%", y = "Na2O + K2O, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3")) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
alk_avg_plot

## Alk average on TAS
tas +
  geom_point(data = gls_avg, aes(x = SiO2, y = Na2O + K2O, shape = RockName, color = RockName)) +
  labs(title = "Rocks Plotted on TAS Diagram", x = "SiO2, Wt%", y = "Na2O + K2O, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4", "blue2", "deeppink2", "orchid3", "royalblue4", "firebrick3", "cyan3")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))

## Mg Avgerage Plot
mg_avg_plot <- gls_avg %>%
  ggplot(mapping = aes(SiO2, MgN, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName), size = 3) +
  labs(title = "Average Silica vs Magnesium Number by rock", x = "SiO2, Wt%", y = "Mg #") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) +  
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3", 
                                "royalblue4", "firebrick3", "cyan3"))

mg_avg_plot <- mg_avg_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) + 
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
mg_avg_plot + scale_x_continuous(limits = c(55,80))

## Iron Average Plot
fe_avg_plot <- gls_avg %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, FeO, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Average Silica vs Iron by Rock", x = "SiO2, Wt%", y = "FeO*, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3"))
fe_avg_plot <- fe_avg_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
fe_avg_plot + scale_x_continuous(limits = c(50, 80))

## Calcium Average Plot
ca_avg_plot <- gls_avg %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, CaO, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Average Silica vs Calcium by Rock", x = "SiO2, Wt%", y = "CaO, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3"))
ca_avg_plot <- ca_avg_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) + 
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
ca_avg_plot + scale_x_continuous(limits = c(50, 80))

##Aluminum Average plot
al_avg_plot <- gls_avg %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, Al2O3, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Average Silica vs Aluminum by Rock", x = "SiO2, Wt%", y = "Al2O3, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3"))
al_avg_plot <- al_avg_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) + 
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
al_avg_plot + scale_x_continuous(limits = c(50, 80))

## Al vs Ti average plot
alti_avg_plot <- gls_avg %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(Al2O3, TiO2, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Average Aluminum vs Titanium by Rock", x = "Al2O3, Wt%", y = "TiO2, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3"))
alti_avg_plot <- alti_avg_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
alti_avg_plot

#attempt at table showing mean and sd of al vs ti using huxtable
al_ti_tb <- rock_data_wt %>%
  group_by(RockName) %>%
  summarize(Al2O3mn = mean(Al2O3), Al2O3sd = sd(Al2O3), 
            TiO2mn = mean(TiO2), TiO2sd = sd(TiO2))

alti_hux <- hux(al_ti_tb) %>%
  add_colnames() %>%
  set_bold(row = 1, col = everywhere, value = TRUE) %>%
  set_number_format(2:9, 2:5, 3) %>%
  set_col_width(1) %>%
  set_all_borders(TRUE)
alti_hux

# create table comparing representative glass samples
cr_data <- cr_data %>%
  select("Comment", "SiO2":"S", "P2O5", "Total")
cr_data2 <- cr_data[,-1]  
rownames(cr_data2) <- cr_data[,1] 
cr_data2 <- as.matrix(cr_data2)  
cr_data2_trans <- t(cr_data2)  
cr_data2_trans2 <- as.data.frame(cr_data2_trans)

cr_table1 <- cr_data2_trans2 %>% 
  select("CR1A2_2 Pt1", "CR1B_1 Pt15", "CR2A2_3 PT1", "CR2B2_1 PT2",
         "CR31_3 PT7", "CR42_1 PT6", "CR51_2 PT5", "CR72_3 Pt1") %>%
  drop_na()
cr_table1_tb <- rownames_to_column(cr_table1, var = "wt %") %>%
  as.tibble()
cr_table1_tb


## Plot 1A to 1B - Si to Alkali
cr1a_cr1b_alk <- rock_data_wt %>%
  filter(grepl("CR1", rock_data_wt$RockName)) %>%
  ggplot(aes(SiO2, Na2O + K2O, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Comparision of CR1A to CR1B Silica to Alkali", x = "SiO2, Wt%", y = "Na2O + K2O, Wt%") +
  scale_shape_manual(values = c(7, 8)) + 
  scale_color_manual(values = c("coral1", "chartreuse3")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
cr1a_cr1b_alk + scale_x_continuous(limits = c(50, 80))

## Plot 1A to 1B - Si to Mg
cr1a_cr1b_mg <- rock_data_wt %>%
  filter(grepl("CR1", rock_data_wt$RockName)) %>%
  ggplot(aes(SiO2, MgN, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Comparision of CR1A to CR1B Silica to Mg#", x = "SiO2, Wt%", y = "Mg #") +
  scale_shape_manual(values = c(7, 8)) + 
  scale_color_manual(values = c("coral1", "chartreuse3")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
cr1a_cr1b_mg + scale_x_continuous(limits = c(50, 80))

## Plot 1A to 1B - Si to Fe
cr1a_cr1b_fe <- rock_data_wt %>%
  filter(grepl("CR1", rock_data_wt$RockName)) %>%
  ggplot(aes(SiO2, FeO, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Comparision of CR1A to CR1B Silica to Iron", x = "SiO2, Wt%", y = "FeO*, Wt%") +
  scale_shape_manual(values = c(7, 8)) + 
  scale_color_manual(values = c("coral1", "chartreuse3")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
cr1a_cr1b_fe + scale_x_continuous(limits = c(50, 80))


## Plot 1A to 1B - Al to Ti
cr1a_cr1b_alti <- rock_data_wt %>%
  filter(grepl("CR1", rock_data_wt$RockName)) %>%
  ggplot(aes(Al2O3, TiO2, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  scale_shape_manual(values = c(7, 8)) + 
  scale_color_manual(values = c("coral1", "chartreuse3")) + 
  geom_point()
cr1a_cr1b_alti


## table for mean and sd of al & ti
al_ti_tb <- rock_data_wt %>%
  group_by(RockName) %>%
  summarize(Al2O3mn = mean(Al2O3), Al2O3sd = sd(Al2O3), 
            TiO2mn = mean(TiO2), TiO2sd = sd(TiO2))

al_ti_tb$RockName <- as.character(al_ti_tb$RockName)

## create function for se
std <- function(x) sd(x)/sqrt(length(x))
std(rock_data_wt$SiO2)  #shows this function works


# df with se
se_data <- rock_data_wt %>%
  group_by(RockName) %>%
  summarize("SiO2 se" = std(SiO2), "TiO2 se" = std(TiO2),
            "Al2O3 se" = std(Al2O3), "Cr se" = std(Cr2O3),
            "MgO se" = std(MgO), "CaO se" = std(CaO), "MnO se" = std(MnO),
            "FeO se" = std(FeO), "Na2O se" = std(Na2O), "K2O se" = std(K2O),
            "S se" = std(S), "P2O5 se" = std(P2O5), "MgN se" = std(MgN),
            "Total se" = std(Total), .groups = "keep")

## df with standard dev 
sd_data <- rock_data_wt %>%
  group_by(RockName) %>%
  summarize("SiO2 sd" = sd(SiO2), "TiO2 sd" = sd(TiO2),
            "Al2O3 sd" = sd(Al2O3), "Cr sd" = sd(Cr2O3),
            "MgO sd" = sd(MgO), "CaO sd" = sd(CaO), "MnO sd" = sd(MnO),
            "FeO sd" = sd(FeO), "Na2O sd" = sd(Na2O), "K2O sd" = sd(K2O),
            "S sd" = sd(S), "P2O5 sd" = sd(P2O5), "MgN sd" = sd(MgN),
            "Total sd" = sd(Total), .groups = "keep")


#table for mean for glass samples
glass_avg <- rock_data_wt %>%
  group_by(RockName) %>% ##break into ind rocks
  summarise(n = n(), "SiO2" = mean(SiO2), "TiO2" = mean(TiO2),
            "Al2O3" = mean(Al2O3), "Cr" = mean(Cr2O3),
            "MgO" = mean(MgO), "CaO" = mean(CaO), "MnO" = mean(MnO),
            "FeO" = mean(FeO), "Na2O" = mean(Na2O), "K2O" = mean(K2O),
            "S" = mean(S), "P2O5" = mean(P2O5), "MgN" = mean(MgN),
            "Total" = mean(Total), .groups = ("keep"))
glass_avg <- as.data.frame(glass_avg)  ##convert above to df
glass_avga <- glass_avg[,-1] ##remove first column from df
rownames(glass_avga) <- glass_avg[, 1] ## add column back in as rownames
glass_avg_tran <- transpose(glass_avga) ##transpose df
rownames(glass_avg_tran) <- colnames(glass_avga) ##trans the col names
colnames(glass_avg_tran) <- rownames(glass_avga) ##trans the row names
rownames_to_column(glass_avg_tran, var = "Element")

## join df for mean with df for sd
join_m_sd <- left_join(glass_avg, sd_data, by = "RockName") %>%
  select(1,2,3,17,4,18,5,19,6,20,7,21,8,22,9,23,10,24,11,25,12,26,13,27,14,28,15,29,16,30)
glass_msd <- as.data.frame(join_m_sd)


## plot iron averages with error bars
fe_glass_msd <- glass_msd %>%
  ggplot(aes(x = SiO2, y = FeO, ymin = FeO - `FeO sd`, ymax = FeO + `FeO sd`, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName), size = 4) +
  geom_errorbar() +
  labs(title = "Bulk Average Silica vs Iron by rock", x = "SiO2, Wt%", y = "FeO, wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) +  
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3", 
                                "royalblue4", "firebrick3", "cyan3"))

fe_glass_msd <- fe_glass_msd + 
  guides(color = guide_legend(override.aes = list(size = 5))) + 
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
fe_glass_msd



  

alti_hux <- hux(al_ti_tb) %>%
  add_colnames() %>%
  set_bold(row = 1, col = everywhere, value = TRUE) %>%
  set_number_format(2:9, 2:5, 2) %>%
  set_all_borders(TRUE) %>%
  set_col_width(4)
print(alti_hux)

##create table w/ chosen glass samples - keep
cr_data <- cr_data %>%
  select("Comment", "P2O5":"Total")
cr_data2 <- cr_data[,-1]  ##removes first column
rownames(cr_data2) <- cr_data[,1] ## adds first column back as proper row
cr_data2 <- as.matrix(cr_data2)  ##changes df to matrix in prep for t()
cr_data2_trans <- t(cr_data2)  ##transposes matrix
cr_data2_trans2 <- as.data.frame(cr_data2_trans)

cr_table1 <- cr_data2_trans2 %>%  ##creates table w/ specific samples chosen
  select("CR1A2_2 Pt1", "CR1B_1 Pt15", "CR2A2_3 PT1", "CR2B2_1 PT2", "CR31_3 PT7", "CR42_1 PT6", "CR51_2 PT5", "CR72_3 Pt1") %>%
  drop_na() ##drops the rows w/ na


