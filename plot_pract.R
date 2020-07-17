#####
#practice#
#####

library(ggplot2)
library(gridExtra)
library(ggpubr)
library(dplyr)
library(arsenal) ## not helpful
library(furniture)

bulk_data <- read.csv("C:/Users/labry/Documents/R/costarican_glass1/data/bulk_comp_data_a.csv")
allan_data <- read.csv("C:/Users/labry/Documents/R/costarican_glass1/data/allan_CR1A.csv")
  #Create new columns for RockName
bulk_data$Comment <- as.character(bulk_data$Comment)
blk_cr1a <- bulk_data %>%
    filter(grepl("CR1A", bulk_data$Comment)) %>% #filter for rock
    mutate("RockName" = "CR1A")  #add column for rockname
blk_cor1a <- bulk_data %>%
    filter(grepl("COR1A", bulk_data$Comment)) %>%
    mutate("RockName" = "COR1A")
blk_cra1a <- bulk_data %>%
    filter(grepl("CRA1A", bulk_data$Comment)) %>%
    mutate("RockName" = "CRA1A")

blk_data <- rbind(blk_cr1a, blk_cor1a, blk_cra1a) #pull all df's together w/ 1 col for rockname
blk_data <- blk_data[,c(29, 23, 2:15)] #reorder the columns and limit to relevant ones
blk_data <- blk_data %>%  #add column w/ Mg# calculated
    mutate("MgN" = ((MgO / (40.31)) / ((MgO / (40.31)) + (FeO / (71.85)))) * (100))
blk_data$RockName <- as.factor(blk_data$RockName) #coerce col to factor (I forget why this was needed)
  
  
blk_data_wt <- blk_data %>%
    select("RockName":"Total", "MgN") %>%  #choose relevant columns
    filter(Total > 98.0 & Total < 101.0) #select rows based on Total
  #leave off last line of this due to bulk not requiring as many constraints

allan_data$Comment <- as.character(allan_data$Comment)
allan_cr1a_bp <- allan_data %>%
  filter(grepl("CRA1A bp", allan_data$Comment)) %>% #filter for rock
  mutate("RockName" = "CR1A bp")
allan_cr1a_sp <- allan_data %>%
  filter(grepl("CRA1A sp", allan_data$Comment)) %>% #filter for rock
  mutate("RockName" = "CR1A sp")
allan_test <- allan_data %>%
  filter(grepl("USGS", allan_data$Comment)) %>% #filter for rock
  mutate("RockName" = "USGS")
allan_data <- rbind(allan_cr1a_bp, allan_cr1a_sp, allan_test) #pull all df's together w/ 1 col for rockname
allan_data <- allan_data[,c(29, 23, 2:15)] #reorder the columns and limit to relevant ones
allan_data <- allan_data %>%  #add column w/ Mg# calculated
  mutate("MgN" = ((MgO / (40.31)) / ((MgO / (40.31)) + (FeO / (71.85)))) * (100))
allan_data$RockName <- as.factor(allan_data$RockName)

allan_data_wt <- allan_data %>%
  select("RockName":"Total", "MgN") %>%  #choose relevant columns
  filter(Total > 98.0 & Total < 101.0)

## find way to enter polynomic equation for Alkali vs Subalkali
A <- blk_data_wt$Na2O + blk_data_wt$K2O

poly <- -3.3539e-4 * A^6 + 1.2030e-2 * A^5 - 1.5188e-1 * A^4 +
  8.6096e-1 * A^3 - 2.1111 * A^2 + 3.9492 * A + 39.0

blk_data_wt$SiO2 >= poly

blk_data_wt$SiO2 >= poly

##find way to create function for plot themese
plot_theme <- function(...){
    guides(color = guide_legend(override.aes = list(size = 5))) + 
    theme(text = element_text(size = 15),
          legend.key.size = unit(1.0, "cm"),
          legend.title = element_text(size = 14),
          plot.title = element_text(hjust = 0.5))
}

##attempt to use function in plot
mg_plot_func <- blk_data_wt %>%
  group_by(RockName) %>%
  ggplot(mapping = aes(SiO2, MgN, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Silica vs Magnesium Number by rock", x = "SiO2, Wt%", y = "Mg #") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) +  
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4", "blue2", "deeppink2", "orchid3", "royalblue4", "firebrick3", "cyan3"))
mg_blk_plot <- mg_blk_plot + 
  guides(color = guide_legend(override.aes = list(size = 5))) + 
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
mg_blk_plot + scale_x_continuous(limits = c(45, 65))
plot_theme(mg_plot_func)




## find way to plot averages on all rather than all points
blk_avg <- blk_data_wt %>% #first create new table containing avg's
  group_by(RockName) %>%
  summarise("SiO2" = mean(SiO2), "TiO2" = mean(TiO2),
            "Al2O3" = mean(Al2O3), "Cr" = mean(Cr2O3),
            "MgO" = mean(MgO), "CaO" = mean(CaO), "MnO" = mean(MnO),
            "FeO" = mean(FeO), "Na2O" = mean(Na2O), "K2O" = mean(K2O),
            "S" = mean(S), "P2O5" = mean(P2O5), "MgN" = mean(MgN),
            "Total" = mean(Total))


#now try against plot
mg_avg_plot <- blk_avg %>%
  ggplot(mapping = aes(SiO2, MgN, colour = RockName)) +
  geom_point(aes(shape = RockName, color = RockName), size = 3) +
  labs(title = "Silica vs Magnesium Number Mean by rock", x = "SiO2, Wt%", y = "Mg #") +
  scale_shape_manual(values = c(7, 8, 10, 11, 21:25)) +  
  scale_color_manual(values = c("coral1", "chartreuse3", "peachpuff4",
                                "blue2", "deeppink2", "orchid3", 
                                "royalblue4", "firebrick3", "cyan3")) +
  guides(color = guide_legend(override.aes = list(size = 5))) + 
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))



## compare orig smpl data (cr) to new samples (cor)  
cr1A_cra1A_blkalk <- blk_data_wt %>%
    filter(grepl("CR[1A]", blk_data_wt$RockName, ignore.case = TRUE)) %>%
    ggplot(aes(SiO2, Na2O + K2O, colour = RockName)) + 
    geom_point(aes(shape = RockName, color = RockName, size = 3)) +
    labs(title = "Silica vs Alkali for CR1 & CRA1A", x = "SiO2, Wt%", y = "Na2O + K2O, Wt%") +
    scale_shape_manual(values = c(7, 10)) + 
    scale_color_manual(values = c("coral1", "blue2")) + 
    guides(color = guide_legend(override.aes = list(size = 5))) +
    theme(text = element_text(size = 15),
          legend.key.size = unit(1.0, "cm"),
          legend.title = element_text(size = 14),
          plot.title = element_text(hjust = 0.5))
cr1A_cra1A_blkalk


allan_bpspalk <- allan_data_wt %>%
  filter(grepl("1A", allan_data_wt$RockName, ignore.case = TRUE)) %>%
  ggplot(aes(SiO2, Na2O + K2O, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Silica vs Alkali for CR1A for bp vs sp", x = "SiO2, Wt%", y = "Na2O + K2O, Wt%") +
  scale_shape_manual(values = c(7, 10)) + 
  scale_color_manual(values = c("coral1", "blue2")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
allan_bpspalk


cr6_cor6_blkalk <- blk_data_wt %>%
  filter(grepl("6", blk_data_wt$RockName)) %>%
  ggplot(aes(SiO2, Na2O + K2O, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Silica vs Alkali for CR6 & COR6", x = "SiO2, Wt%", y = "Na2O + K2O, Wt%") +
  scale_shape_manual(values = c(7, 10)) + 
  scale_color_manual(values = c("coral1", "blue2")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
cr6_cor6_blkalk

cr5_cor5_blkalk <- blk_data_wt %>%
  filter(grepl("5", blk_data_wt$RockName)) %>%
  ggplot(aes(SiO2, Na2O + K2O, colour = RockName)) + 
  geom_point(aes(shape = RockName, color = RockName, size = 3)) +
  labs(title = "Silica vs Alkali for CR5 & COR5", x = "SiO2, Wt%", y = "Na2O + K2O, Wt%") +
  scale_shape_manual(values = c(7, 10)) + 
  scale_color_manual(values = c("coral1", "blue2")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(text = element_text(size = 15),
        legend.key.size = unit(1.0, "cm"),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
cr5_cor5_blkalk
  
## Compare averages and sd's of new samples to old
mean <- blk_data_wt %>%
  group_by(RockName) %>%
  summarise(n = n(),
            "M SiO2" = mean(SiO2), "SD SiO2" = sd(SiO2),
            "M TiO2" = mean(TiO2), "SD TiO2" = sd(TiO2),
            "M Al2O3" = mean(Al2O3), "SD Al2O3" = sd(Al2O3), .groups = "keep")
print(mean)

  
  #using arsenal package to create table
  #this didn't really work, no way to list elements well
fac <- c(blk_data_wt$SiO2, blk_data_wt$P2O5, blk_data_wt$TiO2)
  table_1 <- tableby(RockName ~ fac, data = blk_data_wt)
 summary(table_1, text = TRUE)
 
 #using furniture package to create table
 #this method worked but c/n print well in Rmd so disregarding
 table_2 <- furniture::table1(blk_data_wt,
                    "Elements" = SiO2:Total,
                    splitby = ~RockName,
                    test = FALSE)

##test to compare CR2A_T to CR2A_M
 cr2a_test_alk <- blk_data_wt %>%
   filter(grepl("CR2A", blk_data_wt$RockName)) %>%
   ggplot(aes(SiO2, Na2O + K2O, colour = RockName)) + 
   geom_point(aes(shape = RockName, color = RockName)) +
   scale_shape_manual(values = c(7, 8)) + 
   scale_color_manual(values = c("coral1", "chartreuse3")) + 
   guides(color = guide_legend(override.aes = list(size = 5))) +
   theme(text = element_text(size = 15),
         legend.key.size = unit(1.0, "cm"),
         legend.title = element_text(size = 14))
 cr2a_test_alk
 
 
 ##Create plot for looking at CR1A to CR1B Si to Mg# in bulk rock composition 
 cr2a_test_mg <- blk_data_wt %>%
   filter(grepl("CR2A", blk_data_wt$RockName)) %>%
   ggplot(aes(SiO2, MgN, colour = RockName)) + 
   geom_point(aes(shape = RockName, color = RockName)) +
   scale_shape_manual(values = c(7, 8)) + 
   scale_color_manual(values = c("coral1", "chartreuse3")) + 
   guides(color = guide_legend(override.aes = list(size = 5))) +
   theme(text = element_text(size = 15),
         legend.key.size = unit(1.0, "cm"),
         legend.title = element_text(size = 14))
 cr2a_test_mg
 
 
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
