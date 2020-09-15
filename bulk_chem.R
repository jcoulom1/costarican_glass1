#####
#Bulk Chemistry Analysis#
#####


#Load packages and data
library(dplyr)
library(tidyverse)
library(gridExtra)
library(data.table)
library(captioner)
library(furniture)
library(ggpubr)
library(flextable)

bulk_data <- read.csv("C:/Users/labry/Documents/R/costarican_glass1/data/bulk_comp_data_a.csv")
table_nums <- captioner(prefix = "Table")
figure_nums <- captioner(prefix = "Figure")

figure_nums(name = "tas", caption = "TAS Diagram for bulk rock compositions")
figure_nums(name = "plots", caption = "Major elements plotted against SiO2.  All Wt% except Mg#")
table_nums(name = "mean", caption = "Mean averages for major elements")


## create function for plot symbols
plot_symbols <- list(
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)),  #provide values for shape
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3")) #prov values for color
)

## create function for all theme data
plot_theme <- list(
  guides(color = guide_legend(override.aes = list(size = 5))) + #this makes the legend key marks bigger
    theme(text = element_text(size = 15), #this makes all the text bigger
          legend.key.size = unit(1.0, "cm"), #this makes legend smaller
          legend.title = element_text(size = 14),  #this makes leg text smaller
          plot.title = element_text(hjust = 0.5)) #moves title to center of plot
)

#Create new columns for RockName
bulk_data$Comment <- as.character(bulk_data$Comment)
blk_cr1a <- bulk_data %>%
  filter(grepl("CRA1A", bulk_data$Comment)) %>% #filter for rock
  mutate("RockName" = "CR1A")  #add column for rockname
blk_cr1b <- bulk_data %>%
  filter(grepl("CR1B", bulk_data$Comment)) %>%
  mutate("RockName" = "CR1B")
blk_cr2a_t <- bulk_data %>%
  filter(grepl("CR2A_T", bulk_data$Comment)) %>%
  mutate("RockName" = "CR2A_T")
blk_cr2a_m <- bulk_data %>%
  filter(grepl("CR2A_M", bulk_data$Comment)) %>%
  mutate("RockName" = "CR2A")
blk_cr2b <- bulk_data %>%
  filter(grepl("CR2B", bulk_data$Comment)) %>%
  mutate("RockName" = "CR2B")
blk_cr3 <- bulk_data %>%
  filter(grepl("CR3", bulk_data$Comment)) %>%
  mutate("RockName" = "CR3")
blk_cr4 <- bulk_data %>%
  filter(grepl("CR4", bulk_data$Comment)) %>%
  mutate("RockName" = "CR4")
blk_cr5 <- bulk_data %>%
  filter(grepl("COR5", bulk_data$Comment)) %>%
  mutate("RockName" = "CR5")
blk_cr6 <- bulk_data %>%
  filter(grepl("COR6", bulk_data$Comment)) %>%
  mutate("RockName" = "CR6")
blk_cr7 <- bulk_data %>%
  filter(grepl("CR7", bulk_data$Comment)) %>%
  mutate("RockName" = "CR7")
blk_data <- rbind(blk_cr1a, blk_cr1b, blk_cr2a_m, blk_cr2b, blk_cr3, blk_cr4, blk_cr5, blk_cr6, blk_cr7) #pull all df's together w/ 1 col for rockname
blk_data <- blk_data[,c(29, 23, 2:15)] #reorder the columns and limit to relevant ones
blk_data <- blk_data %>%  #add column w/ Mg# calculated
  mutate("MgN" = ((MgO / (40.31)) / ((MgO / (40.31)) + (FeO / (71.85)))) * (100))
blk_data$RockName <- as.factor(blk_data$RockName) #coerce col to factor (I forget why this was needed)


blk_data_wt <- blk_data %>%
  select("RockName":"Total", "MgN") %>%  #choose relevant columns
  filter(Total > 98.0 & Total < 101.0) #select rows based on Total constraints
#leave off last line of this due to bulk not requiring as many constraints as glass

## average Mo by rock
bulk_mo <- blk_data_wt %>%
  group_by(RockName) %>%
  summarise("Mo m" = mean(Mo), "Mo sd" = sd(Mo), .groups = "keep")
bulk_mo

## what does MgO vs K2O look like?
mg_k20_plot <- blk_data_wt %>%
  ggplot(mapping = aes(MgO, K2O)) +
  facet_wrap(vars(RockName)) +
  geom_point(aes(shape = RockName, color = RockName)) +
  labs(x = "MgO Wt%", y = "K2O Wt%") +
  plot_symbols
mg_k20_plot


mg_CaO_plot <- blk_data_wt %>%
  ggplot(mapping = aes(MgO, CaO)) +
  facet_wrap(vars(RockName)) +
  geom_point(aes(shape = RockName, color = RockName)) +
  labs(x = "MgO Wt%", y = "CaO Wt%") +
  plot_symbols
mg_CaO_plot


##Create plot for SiO2 vs Na2O + K2O by each bulk rock
alk_blk_plot <- ggplot(blk_data_wt, aes(x = SiO2, y = Na2O + K2O)) +
  facet_wrap(vars(RockName)) +  #create facet wrap by rockname
  geom_point(aes(shape = RockName, color = RockName)) +  #indicate using shape and color to visualize
  labs(title = "Silica Vs Alkali (Na2O + K2O)", x = "SiO2, Wt%", y = "Na2O + K20, Wt%") +   #add plot title % axis labels
  plot_symbols +
  plot_theme
alk_blk_plot



##Create plot for SiO2 vs Mg# by each bulk rock
mg_blk_all <- ggplot(blk_data_wt, aes(x = SiO2, y = MgN)) +
  facet_wrap(vars(RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  labs(title = "Silica vs Magnesium #", x = "SiO2, Wt%", y = "Mg #") +
  plot_symbols +
  plot_theme
mg_blk_all



##Create plot for SiO2 vs FeO by each bulk rock
fe_blk_all <- ggplot(blk_data_wt, aes(x = SiO2, y = FeO)) +
  facet_wrap(vars(RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  labs(title = "Silica vs Iron", x = "SiO2, Wt%", y = "FeO*, Wt%") +
  plot_symbols +
  plot_theme
fe_blk_all



#Create plot for SiO2 vs CaO by each bulk rock    
ca_blk_all <- ggplot(blk_data_wt, aes(x = SiO2, y = CaO)) +
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
ca_blk_all



#Create plot for SiO2 vs K by each bulk rock
k_blk_all <- ggplot(blk_data_wt, aes(x = SiO2, y = K2O)) +
  facet_wrap(vars(RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  labs(title = "Silica vs Potassium", x = "SiO2, Wt%", y = "K2O, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) +  
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3")) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
k_blk_all


##Create plot for SiO2 vs Ti by each bulk rock
ti_blk_all <- ggplot(blk_data_wt, aes(x = SiO2, y = TiO2)) +
  facet_wrap(vars(RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  labs(title = "Silica vs Titanium", x = "SiO2, Wt%", y = "TiO2, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) +  
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3")) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
ti_blk_all 


##Create plot for SiO2 vs Al2O3 by each bulk rock
al_blk_all <- ggplot(blk_data_wt, aes(x = SiO2, y = Al2O3)) +
  facet_wrap(vars(RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  labs(title = "Silica vs Aluminum", x = "SiO2, Wt%", y =  "Al2O3, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) +  
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3", 
                                "royalblue4", "firebrick3", "cyan3")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5)) 
al_blk_all

## Al vs Ti bulk data
alti_blk_all <- ggplot(blk_data_wt, aes(x = Al2O3, y = TiO2)) +
  facet_wrap(vars(RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  labs(title = "Aluminum vs Titanium", x = "Al2O3, Wt%", y = "TiO2, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) +  
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3", 
                                "royalblue4", "firebrick3", "cyan3")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14))
alti_blk_all


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

##Plot Alkali by Rock Name - Bulk Composition
final_blk_alkplot <- blk_data_wt %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(x = SiO2, y = Na2O + K2O, colour = RockName, legend(cex = 0.75))) +
  geom_point(aes(shape = RockName, color = RockName)) +
  labs(title = "Silica vs Alkali", x = "SiO2, Wt%", y = "Na2O + K2O, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) +  
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3", 
                                "royalblue4", "firebrick3", "cyan3")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
final_blk_alkplot


#overlay the alkali plot onto TAS diagram
tas +
  geom_point(data = blk_data_wt, aes(x = SiO2, y = Na2O + K2O, shape = RockName, color = RockName)) +
  labs(title = "Bulk Composition on TAS Diagram", x = "SiO2, Wt%", y = "Na2O + K2O") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) +  
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3", 
                                "royalblue4", "firebrick3", "paleturquoise1")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))


##Now to look at plots of all rocks together

##Plot Silica vs Mg# by rock
mg_blk_plot <- blk_data_wt %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, MgN, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName), size = 3) +
  labs(title = "Silica vs Magnesium Number by rock", x = "SiO2, Wt%", y = "Mg #") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) +  
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3", 
                                "royalblue4", "firebrick3", "cyan3"))
mg_blk_plot <- mg_blk_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) + 
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
mg_blk_plot + scale_x_continuous(limits = c(45, 65)) #sets x axis boundaries



##Plot Silica vs Iron by rock
fe_blk_plot <- blk_data_wt %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, FeO, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName), size = 3) +
  labs(title = "Silica vs Iron by rock", x = "SiO2, Wt%", y = "FeO*, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) +  
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3", 
                                "royalblue4", "firebrick3", "cyan3"))
fe_blk_plot <- fe_blk_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
fe_blk_plot + scale_x_continuous(limits = c(45, 65))



##Plot Silica vs Calcium by rock
ca_blk_plot <- blk_data_wt %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, CaO, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName), size = 3) +
  labs(title = "Silica vs Calcium by rock", x = "SiO2, Wt%", y = "CaO, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) +  
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3", 
                                "royalblue4", "firebrick3", "cyan3"))
ca_blk_plot <- ca_blk_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
ca_blk_plot + scale_x_continuous(limits = c(45, 65))



##Plot Silica vs Potassium by rock
k_blk_plot <- blk_data_wt %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, K2O, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Silica vs Potassium by rock", x = "SiO2, Wt%", y = "K2O, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) +  
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3"))
k_blk_plot <- k_blk_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
k_blk_plot + scale_x_continuous(limits = c(45, 65))


##Plot Silica vs Titanium by rock
ti_blk_plot <- blk_data_wt %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, TiO2, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Silica vs Titanium by rock", x = "SiO2, Wt%", y = "TiO2, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) +  
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3"))
ti_blk_plot <- ti_blk_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
ti_blk_plot + scale_x_continuous(limits = c(45, 65))


##Plot Silica vs Aluminum by rock
al_blk_plot <- blk_data_wt %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, Al2O3, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName), size = 3) +
  labs(title = "Silica vs Aluminum by rock", x = "SiO2, Wt%", y = "Al2O3, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) +  
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3", 
                                "royalblue4", "firebrick3", "cyan3"))
al_blk_plot <- al_blk_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
al_blk_plot + scale_x_continuous(limits = c(45, 65))


##Plot Aluminum vs Titanium by rock  
alti_blk_plot <- blk_data_wt %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(Al2O3, TiO2, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName), size = 3) +
  labs(title = "Aluminum vs Titanium by rock", x = "Al2O3, Wt%", y = "TiO2, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) +  
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3", 
                                "royalblue4", "firebrick3", "cyan3"))
alti_blk_plot <- alti_blk_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
alti_blk_plot


## Pull plots together into one figure using gridExtra
grid.arrange(final_blk_alkplot, mg_blk_plot, fe_blk_plot, alti_blk_plot, ncol = 2) +
  facet_wrap(vars(RockName))

## Pull plots together into one figure using ggPubr
ggarrange(mg_blk_plotp, fe_blk_plotp, ca_blk_plotp, k_blk_plotp, ti_blk_plotp, al_blk_plotp,
          ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")  ##this looks better
#but requires first plot to include info and balance to not (see rmd for plots)


#can I make a function for mean
x <- blk_data_wt$SiO2

mean_func <- function(x){
  mean_d <- c(mean(x), sd(x))
}

mean_func()

## create table of bulk averages by rock
blk_avg <- blk_data_wt %>%
  group_by(RockName) %>%
  summarise("SiO2" = c(mean(SiO2), sd(SiO2)), "TiO2" = c(mean(TiO2), sd(TiO2)),
            "Al2O3" = c(mean(Al2O3), sd(Al2O3)), "Cr" = c(mean(Cr2O3), sd(Cr2O3)),
            "MgO" = c(mean(MgO), sd(MgO)), "CaO" = c(mean(CaO), sd(CaO)), 
            "MnO" = c(mean(MnO), sd(MnO)),"FeO" = c(mean(FeO), sd(FeO)),
            "Na2O" = c(mean(Na2O), "K2O" = mean(K2O),
            "S" = mean(S), "P2O5" = mean(P2O5), "MgN" = mean(MgN),
            "Total" = mean(Total), .groups = "keep")


alk_avg_blkplot <- blk_avg %>%
  ggplot(mapping = aes(x = SiO2, y = Na2O + K2O, colour = RockName, legend(cex = 0.75))) +
  geom_point(aes(shape = RockName, color = RockName, size = 2)) +
  labs(title = "Average Glass Alkali per Rock", x = "SiO2, Wt%", y = "Na2O + K2O, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3")) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
alk_avg_blkplot


##plot average alkali
tas +
  geom_point(data = blk_avg, aes(x = SiO2, y = Na2O + K2O, shape = RockName, color = RockName)) +
  labs(title = "Bulk Rocks Plotted on TAS Diagram", x = "SiO2, Wt%", y = "Na2O + K2O, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4", "blue2", "deeppink2", "orchid3", "royalblue4", "firebrick3", "cyan3")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))

## bulk Mg average plot
mg_avg_blkplot <- blk_avg %>%
  ggplot(mapping = aes(SiO2, MgN, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName), size = 3) +
  labs(title = "Bulk Average Silica vs Magnesium Number by rock", x = "SiO2, Wt%", y = "Mg #") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) +  
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3", 
                                "royalblue4", "firebrick3", "cyan3"))

mg_avg_blkplot <- mg_avg_blkplot + 
  guides(color = guide_legend(override.aes = list(size = 5))) + 
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
mg_avg_blkplot + scale_x_continuous(limits = c(50,60))


## Bulk Iron Average Plot
fe_avg_blkplot <- blk_avg %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, FeO, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Bulk Average Silica vs Iron by Rock", x = "SiO2, Wt%", y = "FeO*, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3"))
fe_avg_blkplot <- fe_avg_blkplot + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
fe_avg_blkplot + scale_x_continuous(limits = c(50, 60))


## Bulk Calcium Average Plot
ca_avg_blkplot <- blk_avg %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, CaO, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Bulk Average Silica vs Calcium by Rock", x = "SiO2, Wt%", y = "CaO, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3"))
ca_avg_blkplot <- ca_avg_blkplot + 
  guides(color = guide_legend(override.aes = list(size = 5))) + 
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
ca_avg_blkplot + scale_x_continuous(limits = c(50, 60))


## Bulk Aluminum Average plot
al_avg_blkplot <- blk_avg %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, Al2O3, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Bulk Average Silica vs Aluminum by Rock", x = "SiO2, Wt%", y = "Al2O3, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3"))
al_avg_blkplot <- al_avg_blkplot + 
  guides(color = guide_legend(override.aes = list(size = 5))) + 
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
al_avg_blkplot + scale_x_continuous(limits = c(50, 60))

## Bulk Al vs Ti average plot
alti_avg_blkplot <- blk_avg %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(Al2O3, TiO2, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Bulk Average Aluminum vs Titanium by Rock", x = "Al2O3, Wt%", y = "TiO2, Wt%") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3",
                                "royalblue4", "firebrick3", "cyan3"))
alti_avg_blkplot <- alti_avg_blkplot + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
alti_avg_blkplot


##Create plot for looking at CR1A to CR1B Si to Alkali in bulk rock composition  
cr1a_cr1b_blkalk <- blk_data_wt %>%
  filter(grepl("CR1", blk_data_wt$RockName)) %>%
  ggplot(aes(SiO2, Na2O + K2O, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Silica vs Alkali for CR1", x = "SiO2, Wt%", y = "Na2O + K2O, Wt%") +
  scale_shape_manual(values = c(7, 8)) + 
  scale_color_manual(values = c("coral1", "chartreuse3")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
cr1a_cr1b_blkalk


##Create plot for looking at CR1A to CR1B Si to Mg# in bulk rock composition 
cr1a_cr1b_blkmg <- blk_data_wt %>%
  filter(grepl("CR1", blk_data_wt$RockName)) %>%
  ggplot(aes(SiO2, MgN, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Silica vs Magnesium # for CR1", x = "SiO2, Wt%", y = "Mg #") +
  scale_shape_manual(values = c(7, 8)) + 
  scale_color_manual(values = c("coral1", "chartreuse3")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
cr1a_cr1b_blkmg


##Create plot for looking at CR1A to CR1B Si to Fe in bulk rock composition 
cr1a_cr1b_blkfe <- blk_data_wt %>%
  filter(grepl("CR1", blk_data_wt$RockName)) %>%
  ggplot(aes(SiO2, FeO, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Silica vs Iron for CR1", x = "SiO2, Wt%", y = "FeO*, Wt%") +
  scale_shape_manual(values = c(7, 8)) + 
  scale_color_manual(values = c("coral1", "chartreuse3")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
cr1a_cr1b_blkfe

#Create plot for looking at CR1A to CR1B Al to Ti in bulk rock composition
cr1a_cr1b_blkalti <- blk_data_wt %>%
  filter(grepl("CR1", blk_data_wt$RockName)) %>%
  ggplot(aes(Al2O3, TiO2, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Aluminum vs Titanium for CR1", x = "Al2O3, Wt%", y = "TiO2, Wt%") +
  scale_shape_manual(values = c(7, 8)) + 
  scale_color_manual(values = c("coral1", "chartreuse3")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
cr1a_cr1b_blkalti

## plot CR1 all together in 1 figure
ggarrange(cr1a_cr1b_blkalk, cr1a_cr1b_blkmg, cr1a_cr1b_blkfe, cr1a_cr1b_blkalti, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

##Create plot for looking at CR1A to CR1B Al to Ti in bulk rock composition 
cr1a_cr1b_blkalti <- blk_data_wt %>%
  filter(grepl("CR1", blk_data_wt$RockName)) %>%
  ggplot(aes(Al2O3, TiO2, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  scale_shape_manual(values = c(7, 8)) + 
  scale_color_manual(values = c("coral1", "chartreuse3")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
cr1a_cr1b_blkalti


##Taking a look at comparing CR2A to CR2B in Bulk Composition

cr2a_cr2b_blkalk <- blk_data_wt %>%
  filter(grepl("CR2", blk_data_wt$RockName)) %>%
  ggplot(aes(SiO2, Na2O + K2O, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Silica vs Alkali for CR2", x = "SiO2, Wt%", y = "Na2O + K2O, Wt%") +
  scale_shape_manual(values = c(10, 11)) + 
  scale_color_manual(values = c("peachpuff4", "blue2")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
cr2a_cr2b_blkalk


##Create plot for looking at CR2A to CR2B Si to Mg# in bulk rock composition 
cr2a_cr2b_blkmg <- blk_data_wt %>%
  filter(grepl("CR2", blk_data_wt$RockName)) %>%
  ggplot(aes(SiO2, MgN, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Silica vs Magnesium # for CR2", x = "SiO2, Wt%", y = "Mg #") +
  scale_shape_manual(values = c(10, 11)) + 
  scale_color_manual(values = c("peachpuff4", "blue2")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
cr2a_cr2b_blkmg


##Create plot for looking at CR12 to CR2B Si to Fe in bulk rock composition 
cr2a_cr2b_blkfe <- blk_data_wt %>%
  filter(grepl("CR2", blk_data_wt$RockName)) %>%
  ggplot(aes(SiO2, FeO, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName), size = 3) +
  labs(title = "Silica vs Iron for CR2", x = "SiO2, Wt%", y = "FeO*, Wt%") + 
  scale_shape_manual(values = c(10, 11)) + 
  scale_color_manual(values = c("peachpuff4", "blue2")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
cr2a_cr2b_blkfe


##Create plot for looking at CR2A to CR2B Al to Ti in bulk rock composition 
cr2a_cr2b_blkalti <- blk_data_wt %>%
  filter(grepl("CR2", blk_data_wt$RockName)) %>%
  ggplot(aes(Al2O3, TiO2, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Aluminum vs Titanium for CR2", x = "Al2O3, Wt%", y = "TiO2, Wt%") +
  scale_shape_manual(values = c(10, 11)) + 
  scale_color_manual(values = c("peachpuff4", "blue2")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
cr2a_cr2b_blkalti

##Create plots comparing CR1 to CR2

cr1_cr2_blkalk <- blk_data_wt %>%
  filter(grepl("CR[1-2]", blk_data_wt$RockName)) %>%
  ggplot(aes(SiO2, Na2O + K2O, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Silica vs Alkali for CR1 & CR2", x = "SiO2, Wt%", y = "Na2O + K2O, Wt%") +
  scale_shape_manual(values = c(7, 7, 10, 10)) + 
  scale_color_manual(values = c("coral1", "coral1", "blue2", "blue2")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
cr1_cr2_blkalk


##Create plot for looking at CR1 to CR2 Si to Mg# in bulk rock composition 
cr1_cr2_blkmg <- blk_data_wt %>%
  filter(grepl("CR[1-2]", blk_data_wt$RockName)) %>%
  ggplot(aes(SiO2, MgN, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Silica vs Magnesium # for CR1 & CR2", x = "SiO2, Wt%", y = "Mg #") +
  scale_shape_manual(values = c(7, 7, 10, 10)) + 
  scale_color_manual(values = c("coral1", "coral1", "blue2", "blue2")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
cr1_cr2_blkmg


##Create plot for looking at CR1 to CR2 Si to Fe in bulk rock composition 
cr1_cr2_blkfe <- blk_data_wt %>%
  filter(grepl("CR[1-2]", blk_data_wt$RockName)) %>%
  ggplot(aes(SiO2, FeO, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Silica vs Iron for CR2", x = "SiO2, Wt%", y = "FeO*, Wt%") + 
  scale_shape_manual(values = c(7, 7, 10, 10)) + 
  scale_color_manual(values = c("coral1", "coral1", "blue2", "blue2")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
cr1_cr2_blkfe


##Create plot for looking at CR1 to CR2 Al to Ti in bulk rock composition 
cr1_cr2_blkalti <- blk_data_wt %>%
  filter(grepl("CR[1-2]", blk_data_wt$RockName)) %>%
  ggplot(aes(Al2O3, TiO2, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Aluminum vs Titanium for CR1  & CR2", x = "Al2O3, Wt%", y = "TiO2, Wt%") +
  scale_shape_manual(values = c(7, 7, 10, 10)) + 
  scale_color_manual(values = c("coral1", "coral1", "blue2", "blue2")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
cr1_cr2_blkalti




##Create table for mean for each rock
#I like this better - cleaner but sd not included
bulk_avg <- blk_data_wt %>%
  group_by(RockName) %>% ##break into ind rocks
  summarise(n = n(), "SiO2" = mean(SiO2), "TiO2" = mean(TiO2),
            "Al2O3" = mean(Al2O3), "Cr" = mean(Cr2O3),
            "M MgO" = mean(MgO), "CaO" = mean(CaO), "MnO" = mean(MnO),
            "M FeO" = mean(FeO), "Na2O" = mean(Na2O), "K2O" = mean(K2O),
            "M S" = mean(S), "P2O5" = mean(P2O5), "MgN" = mean(MgN),
            "M Total" = mean(Total), .groups = ("keep"))
bulk_avg <- as.data.frame(bulk_avg)  ##convert above to df
bulk_avga <- bulk_avg[,-1] ##remove first column from df
rownames(bulk_avga) <- bulk_avg[, 1] ## add column back in as rownames
bulk_avg_tran <- transpose(bulk_avga) ##transpose df
rownames(bulk_avg_tran) <- colnames(bulk_avga) ##trans the col names
colnames(bulk_avg_tran) <- rownames(bulk_avga) ##trans the row names
rownames_to_column(bulk_avg_tran, var = "Element")






##Create table for mean and sd for each rock
#I don't care for this
bulk_avg <- blk_data_wt %>%
  group_by(RockName) %>% ##break into ind rocks
  summarise(n = n(),
            "M SiO2" = mean(SiO2), "SD SiO2" = sd(SiO2),
            "M TiO2" = mean(TiO2), "SD TiO2" = sd(TiO2),
            "M Al2O3" = mean(Al2O3), "SD Al2O3" = sd(Al2O3),
            "M Cr" = mean(Cr2O3), "SD Cr" = sd(Cr2O3),
            "M MgO" = mean(MgO), "SD MgO" = sd(MgO),
            "M CaO" = mean(CaO), "SD CaO" = sd(CaO),
            "M MnO" = mean(MnO), "SD MnO" = sd(MnO),
            "M FeO" = mean(FeO), "SD FeO" = sd(FeO),
            "M Na2O" = mean(Na2O), "SD Na2O" = sd(Na2O),
            "M K2O" = mean(K2O),  "SD K2O" = sd(K2O),
            "M S" = mean(S),  "SD S" = sd(S),
            "M P2O5" = mean(P2O5), "SD P2O5" = sd(P2O5),
            "M MgN" = mean(MgN),  "SD MgN" = sd(MgN),
            "M Total" = mean(Total), "SD Total" = sd(Total), .groups = "keep")
bulk_avg2 <- as.data.frame(bulk_avg)  ##convert above to df
bulk_avg3 <- bulk_avg2[,-1] ##remove first column from df
rownames(bulk_avg3) <- bulk_avg2[, 1] ## add column back in as rownames
bulk_avg_tran <- transpose(bulk_avg3) ##transpose df but loses row and col names
rownames(bulk_avg_tran) <- colnames(bulk_avg3) ##puts the rownames back
colnames(bulk_avg_tran) <- rownames(bulk_avg3) ##puts the column names back
rownames_to_column(bulk_avg_tran, var = "Element")

##Use Flextable to hightlight every row
myft <- flextable(
  head(bulk_avg_tran),
)
myft

## attempt to create table with furniture package
#this method worked but c/n print well in Rmd so disregarding

table_2 <- furniture::table1(blk_data_wt,
                             "SiO2" = SiO2, "TiO2" = TiO2,
                             splitby = ~RockName,
                             test = FALSE)
