#####
#practice#
#####

library(ggplot2)
library(gridExtra)
library(ggpubr)
library(dplyr)

  bulk_data <- read.csv("C:/Users/labry/Documents/R/costarican_glass1/data/bulk_comp_data.csv")
  
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
  
  
  ##Plot Silica vs Mg# by rock  (FIRST PLOT)
  mg_blk_plot <- blk_data_wt %>%
    group_by(RockName) %>%
    ggplot(mapping = aes(SiO2, MgN, colour = RockName)) +
    geom_point(aes(shape = RockName, color = RockName)) +
    labs(x = "SiO2 wt%", y = "100 Mg/Mg + Fe2 Wt%") +
    scale_shape_manual(values = c(7, 8, 21:25, 10)) + 
    scale_color_manual(values = c("coral1", "chartreuse3", "turquoise3", "blue2", "deeppink2", "orchid1", "seagreen3", "firebrick3"))
  mg_blk_plot <- mg_blk_plot + 
    guides(color = guide_legend(override.aes = list(size = 5))) +
    theme(text = element_text(size = 10))
  mg_blk_plot
  
  
  
  ##Plot Silica vs Iron by rock
  fe_blk_plot <- blk_data_wt %>%
    group_by(RockName) %>%
    ggplot(mapping = aes(SiO2, FeO, colour = RockName)) +
    geom_point(aes(shape = RockName, color = RockName)) +
    labs(x = "SiO2 Wt%", y = "Fe2O3 Wt%") +
    scale_shape_manual(values = c(7, 8, 21:25, 10)) + 
    scale_color_manual(values = c("coral1", "chartreuse3", "turquoise3", "blue2", "deeppink2", "orchid1", "seagreen3", "firebrick3"))
  fe_blk_plot <- fe_blk_plot + 
    guides(color = guide_legend(override.aes = list(size = 5))) +
    theme(text = element_text(size = 10))
  fe_blk_plot
  
  
  
  ##Plot Silica vs Calcium by rock
  ca_blk_plot <- blk_data_wt %>%
    group_by(RockName) %>%
    ggplot(mapping = aes(SiO2, CaO, colour = RockName)) +
    geom_point(aes(shape = RockName, color = RockName)) +
    labs(x = "SiO2 Wt%", y = "CaO Wt%") +
    scale_shape_manual(values = c(7, 8, 21:25, 10)) + 
    scale_color_manual(values = c("coral1", "chartreuse3", "turquoise3", "blue2", "deeppink2", "orchid1", "seagreen3", "firebrick3"))
  ca_blk_plot <- ca_blk_plot + 
    guides(color = guide_legend(override.aes = list(size = 5))) +
    theme(text = element_text(size = 10))
  ca_blk_plot
  
  
  ##Plot Silica vs Aluminum by rock
  al_blk_plot <- blk_data_wt %>%
    group_by(RockName) %>%
    ggplot(mapping = aes(SiO2, Al2O3, colour = RockName)) +
    geom_point(aes(shape = RockName, color = RockName)) +
    labs(x = "SiO2 Wt%", y = "Al2O3 Wt%") +
    scale_shape_manual(values = c(7, 8, 21:25, 10)) + 
    scale_color_manual(values = c("coral1", "chartreuse3", "turquoise3", "blue2", "deeppink2", "orchid1", "seagreen3", "firebrick3"))
  al_blk_plot <- al_blk_plot + 
    guides(color = guide_legend(override.aes = list(size = 5))) +
    theme(text = element_text(size = 10))
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
  

  
##Create table for mean avgs by rockname for all elements  
blk_df <- read.csv("C:/Users/labry/Documents/R/costarican_glass1/data/bulk_comp_data.csv", row.names = NULL)
blk_df <- blk_df %>%
  select("Comment", "P2O5":"Total")  ##pare down columns
blk_df2 <- blk_df[,-1]  ##removes first column
rownames(blk_df2) <- blk_df[,1] ##puts column back as proper row
blk_df2 <- as.matrix(blk_df2) ##recast as matrix so can be transposed
blk_trans <- t(blk_df2)  ##transpose from (ggpubr package)
blk_trans2 <- as.data.frame(blk_trans)  ##recast as df

blk_table1 <- blk_trans2 %>%
  select(2, 32, 53, 76, 115)


##Elegant way of creating table w/ chosen samples
glass_df <- read.csv("C:/Users/labry/Documents/R/costarican_glass1/data/wt%_probe_data.csv")
glass_df <- glass_df %>%
  select("Comment", "P2O5":"Total")
glass_df2 <- glass_df[,-1]  ##removes first column
rownames(glass_df2) <- glass_df[,1] ## adds first column back as proper row
glass_df2 <- as.matrix(glass_df2)  ##changes df to matrix in prep for t()
glass_trans <- t(glass_df2)  ##transposes matrix
glass_trans2 <- as.data.frame(glass_trans)  ##brings back to df

glass_table1 <- glass_trans2 %>%  ##creates table w/ specific samples chosen
  select("CR1A3_3 PT1", "CR1B_1 Pt15", "CR2A2_3 PT1", "CR2B2_1 PT2", "CR32_2 PT3", "CR43_1 PT5", "CR51_2 PT5", "CR72_2 Pt2") %>%
  drop_na() ##drops the rows w/ na
write.table(glass_table1)

blk_avg <- blk_data_wt %>%
  group_by(RockName) %>%
  summarise(Mean_sio2 = mean(SiO2), Mean_Al2O3 = mean(Al2O3))


##can I take the table created above and transpose rows/columns?  Yes
blk_avg_df <- as.data.frame(blk_avg)

blk_avg2 <- blk_avg_df[,-1] #remove fist column
rownames(blk_avg2) <- blk_avg_df[,1] #adds column back as proper row
blk_avg_mtrx <- as.matrix(blk_avg2) #chgs to matrix so t() is poss
blk_avg_trans <- t(blk_avg_mtrx)

#need to 

library(tidyverse)
mtcars
has_rownames(mtcars)

mtcars_tb <- rownames_to_column(mtcars, var = "car") %>%
  as.tibble()
