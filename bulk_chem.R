#####
#Bulk Chemistry Analysis#
#####


#Load packages and data
library(dbplyr)
library(tidyverse)
library(gridExtra)
library(grid)
library(data.table)
library(lattice)
library(captioner)
library(writexl)

bulk_data <- read.csv("C:/Users/labry/Documents/R/costarican_glass1/data/bulk_comp_data.csv")
table_nums <- captioner(prefix = "Table")
figure_nums <- captioner(prefix = "Figure")

figure_nums(name = "tas", caption = "TAS Diagram for bulk rock compositions")
figure_nums(name = "plots", caption = "Major elements plotted against SiO2.  All Wt% except Mg#")
table_nums(Name = "mean", caption = "Mean averages for major elements")


#Create new columns for RockName
bulk_data$Comment <- as.character(bulk_data$Comment)
blk_cr1a <- bulk_data %>%
  filter(grepl("CR1A", bulk_data$Comment)) %>% #filter for rock
  mutate("RockName" = "CR1A")  #add column for rockname
blk_cr1b <- bulk_data %>%
  filter(grepl("CR1B", bulk_data$Comment)) %>%
  mutate("RockName" = "CR1B")
blk_cr3 <- bulk_data %>%
  filter(grepl("CR3", bulk_data$Comment)) %>%
  mutate("RockName" = "CR3")
blk_cr6 <- bulk_data %>%
  filter(grepl("CR6", bulk_data$Comment)) %>%
  mutate("RockName" = "CR6")
blk_cr7 <- bulk_data %>%
  filter(grepl("CR7", bulk_data$Comment)) %>%
  mutate("RockName" = "CR7")
blk_data <- rbind(blk_cr1a, blk_cr1b, blk_cr3, blk_cr6, blk_cr7) #pull all df's together w/ 1 col for rockname
blk_data <- blk_data[,c(29, 23, 2:15)] #reorder the columns and limit to relevant ones
blk_data <- blk_data %>%  #add column w/ Mg# calculated
  mutate("MgN" = ((MgO / (40.31)) / ((MgO / (40.31)) + (FeO / (71.85)))) * (100))
blk_data$RockName <- as.factor(blk_data$RockName) #coerce col to factor (I forget why this was needed)


blk_data_wt <- blk_data %>%
  select("RockName":"Total", "MgN") %>%  #choose relevant columns
  filter(Total > 98.0 & Total < 101.0) #select rows based on Total
#leave off last line of this due to bulk not requiring as many constraints

##Create plot for SiO2 vs Na2O + K2O by each bulk rock
alk_blk_plot <- ggplot(blk_data_wt, aes(x = SiO2, y = Na2O + K2O)) +
  facet_wrap(vars(RockName)) +  #create facet wrap by rockname
  geom_point(aes(shape = RockName, color = RockName)) +  #indicate using shape and color to visualize
  labs(title = "Silica Vs Alkali (Na2O + K2O)", x = "SiO2, Wt%", y = "Na2O + K20, Wt%") +   #add plot title % axis labels
  scale_shape_manual(values = c(7, 8, 21:25, 10)) +  #provide values for shape
  scale_color_manual(values = c("coral1", "chartreuse3", "turquoise3", "blue2", "deeppink2", "orchid1", "seagreen3", "firebrick3")) + #prov values for color
  guides(color = guide_legend(override.aes = list(size = 5))) + #this makes the legend key marks bigger
  theme(text = element_text(size = 15), #this makes all the text bigger
        legend.key.size = unit(1.0, "cm"), #this makes legend smaller
        legend.title = element_text(size = 14),  #this makes leg text smaller
        plot.title = element_text(hjust = 0.5)) #moves title to center of plot
alk_blk_plot



##Create plot for SiO2 vs Mg# by each bulk rock
mg_blk_all <- ggplot(blk_data_wt, aes(x = SiO2, y = MgN)) +
  facet_wrap(vars(RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  ggtitle("Silica Vs Mg#") +
  scale_shape_manual(values = c(7, 8, 21:25, 10)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "turquoise3", "blue2", "deeppink2", "orchid1", "seagreen3", "firebrick3")) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
mg_blk_all



##Create plot for SiO2 vs FeO by each bulk rock
fe_blk_all <- ggplot(blk_data_wt, aes(x = SiO2, y = FeO)) +
  facet_wrap(vars(RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  ggtitle("Silica vs Iron") +
  scale_shape_manual(values = c(7, 8, 21:25, 10)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "turquoise3", "blue2", "deeppink2", "orchid1", "seagreen3", "firebrick3")) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
fe_blk_all



#Create plot for SiO2 vs CaO by each bulk rock    
ca_blk_all <- ggplot(blk_data_wt, aes(x = SiO2, y = CaO)) +
  facet_wrap(vars(RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  ggtitle("Silica vs Calcium") +
  scale_shape_manual(values = c(7, 8, 21:25, 10)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "turquoise3", "blue2", "deeppink2", "orchid1", "seagreen3", "firebrick3")) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
ca_blk_all


##Create plot for SiO2 vs Al2O3 by each bulk rock
al_blk_all <- ggplot(blk_data_wt, aes(x = SiO2, y = Al2O3)) +
  facet_wrap(vars(RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  ggtitle("Silica vs Aluminum") +
  scale_shape_manual(values = c(7, 8, 21:25, 10)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "turquoise3", "blue2", "deeppink2", "orchid1", "seagreen3", "firebrick3")) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5)) #adds title for plot
al_blk_all

## Al vs Ti bulk data
alti_blk_all <- ggplot(blk_data_wt, aes(x = Al2O3, y = TiO2)) +
  facet_wrap(vars(RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  scale_shape_manual(values = c(7, 8, 21:25, 10)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "turquoise3", "blue2", "deeppink2", "orchid1", "seagreen3", "firebrick3")) +
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
  scale_shape_manual(values = c(7, 8, 21:25, 10)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "turquoise3", "blue2", "deeppink2", "orchid1", "seagreen3", "firebrick3")) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14))
final_blk_alkplot


#overlay the alkali plot onto TAS diagram
tas +
  geom_point(data = blk_data_wt, aes(x = SiO2, y = Na2O + K2O, shape = RockName, color = RockName)) +
  labs(title = "Bulk Composition on TAS Diagram", x = "SiO2, Wt%", y = "Na2O + K2O") +
  scale_shape_manual(values = c(7, 8, 21:25, 10)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "turquoise3", "blue2", "deeppink2", "orchid1", "seagreen3", "firebrick3")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
figure_nums("tas")


##Now to look at plots of all rocks together

##Plot Silica vs Mg# by rock
mg_blk_plot <- blk_data_wt %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, MgN, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName)) +
  ggtitle("Silica vs Magnesium Number by rock") +
  scale_shape_manual(values = c(7, 8, 21:25, 10)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "turquoise3", "blue2", "deeppink2", "orchid1", "seagreen3", "firebrick3"))
mg_blk_plot <- mg_blk_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) + 
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
mg_blk_plot



##Plot Silica vs Iron by rock
fe_blk_plot <- blk_data_wt %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, FeO, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName)) +
  ggtitle("Silica vs Iron by rock") +
  scale_shape_manual(values = c(7, 8, 21:25, 10)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "turquoise3", "blue2", "deeppink2", "orchid1", "seagreen3", "firebrick3"))
fe_blk_plot <- fe_blk_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
fe_blk_plot



##Plot Silica vs Calcium by rock
ca_blk_plot <- blk_data_wt %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, CaO, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName)) +
  ggtitle("Silica vs Calcium by rock") +
  scale_shape_manual(values = c(7, 8, 21:25, 10)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "turquoise3", "blue2", "deeppink2", "orchid1", "seagreen3", "firebrick3"))
ca_blk_plot <- ca_blk_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
ca_blk_plot


##Plot Silica vs Aluminum by rock
al_blk_plot <- blk_data_wt %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, Al2O3, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName)) +
  ggtitle("Silica vs Aluminum by rock") +
  scale_shape_manual(values = c(7, 8, 21:25, 10)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "turquoise3", "blue2", "deeppink2", "orchid1", "seagreen3", "firebrick3"))
al_blk_plot <- al_blk_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
al_blk_plot

##Plot Aluminum vs Titanium by rock  
alti_blk_plot <- blk_data_wt %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(Al2O3, TiO2, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName)) +
  ggtitle("Aluminum vs Titanium by rock") +
  scale_shape_manual(values = c(7, 8, 21:25, 10)) + 
  scale_color_manual(values = c("coral1", "chartreuse3", "turquoise3", "blue2", "deeppink2", "orchid1", "seagreen3", "firebrick3"))
alti_blk_plot <- alti_blk_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
alti_blk_plot


## Pull plots together into one figure
grid.arrange(mg_blk_plot, fe_blk_plot, alk_blk_plot, al_blk_plot, ncol = 2) +
  facet_wrap(vars(RockName))


##Create plot for looking at CR1A to CR1B Si to Alkali in bulk rock composition  
cr1a_cr1b_blkalk <- blk_data_wt %>%
  filter(grepl("CR1", blk_data_wt$RockName)) %>%
  ggplot(aes(SiO2, Na2O + K2O, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  scale_shape_manual(values = c(7, 8)) + 
  scale_color_manual(values = c("coral1", "chartreuse3")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14))
cr1a_cr1b_blkalk


##Create plot for looking at CR1A to CR1B Si to Mg# in bulk rock composition 
cr1a_cr1b_blkmg <- blk_data_wt %>%
  filter(grepl("CR1", blk_data_wt$RockName)) %>%
  ggplot(aes(SiO2, MgN, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  scale_shape_manual(values = c(7, 8)) + 
  scale_color_manual(values = c("coral1", "chartreuse3")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14))
cr1a_cr1b_blkmg


##Create plot for looking at CR1A to CR1B Si to Fe in bulk rock composition 
cr1a_cr1b_blkfe <- blk_data_wt %>%
  filter(grepl("CR1", blk_data_wt$RockName)) %>%
  ggplot(aes(SiO2, FeO, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  scale_shape_manual(values = c(7, 8)) + 
  scale_color_manual(values = c("coral1", "chartreuse3")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14))
cr1a_cr1b_blkfe


##Create plot for looking at CR1A to CR1B Al to Ti in bulk rock composition 
cr1a_cr1b_blkalti <- blk_data_wt %>%
  filter(grepl("CR1", blk_data_wt$RockName)) %>%
  ggplot(aes(Al2O3, TiO2, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName)) +
  scale_shape_manual(values = c(7, 8)) + 
  scale_color_manual(values = c("coral1", "chartreuse3")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14))
cr1a_cr1b_blkalti


##Taking a look at comparing CR2A to CR2B in Bulk Composition
##(bulk data for CR2 not yet available)


##Create table for mean for each rock
bulk_avg <- blk_data_wt %>%
  group_by(RockName) %>% ##break into ind rocks
  summarise(n = n(),
            "M SiO2" = mean(SiO2), "sd SiO2" = (.,sd(SiO2)),
            "M TiO2" = mean(TiO2),
            "M Al2O3" = mean(Al2O3),
            "M Cr" = mean(Cr2O3),
            "M MgO" = mean(MgO),
            "M CaO" = mean(CaO),
            "M MnO" = mean(MnO),
            "M FeO" = mean(FeO),
            "M Na2O" = mean(Na2O),
            "M K2O" = mean(K2O),
            "M S" = mean(S),
            "M P2O5" = mean(P2O5),
            "M MgN" = mean(MgN),
            "M Total" = mean(Total))
bulk_avg <- as.data.frame(bulk_avg)  ##convert above to df
bulk_avga <- bulk_avg[,-1] ##remove first column from df
rownames(bulk_avga) <- bulk_avg[, 1] ## add column back in as rownames
bulk_avg_tran <- transpose(bulk_avga) ##transpose df
rownames(bulk_avg_tran) <- colnames(bulk_avga) ##trans the col names
colnames(bulk_avg_tran) <- rownames(bulk_avga) ##trans the row names
rownames_to_column(bulk_avg_tran, var = "Element")  #give name to row


 
