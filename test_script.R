#####
#using this to test code for test.rmd
#####


# load packages and data for testing
library(dplyr)
library(tidyverse)
my_data <- read.csv("C:/Users/labry/Documents/R/costarican_glass1/data/bulk_comp_data_a.csv")


## create table for testing dimensions
avg <- mtcars %>%
  group_by(cyl) %>%
  summarise(n = n(), "Disp" = mean(disp), "Hp" = mean(hp), "Drat" = mean(drat),
            "Wt" = mean(wt), "Qsec" = mean(qsec), .groups = "keep")
avg

## rearrange table 
avg2 <- as.data.frame(avg)
avg3 <- avg2[,-1]
rownames(avg3) <- avg2[, 1]
avg_trans <- transpose(avg3)
rownames(avg_trans) <- colnames(avg3)
colnames(avg_trans) <- rownames(avg3)
rownames_to_column(avg_trans, var = "Funtions")

## this creates an error in Rmd - "attempt to set 'rownames' on an object with no dimension
#calls

## set up my table which I know works in knit from my doc but subset to get smaller chunck
my_data$Comment <- as.character(my_data$Comment)
my_cr3 <- my_data %>%
  filter(grepl("CR3", my_data$Comment)) %>%
  mutate("RockName" = "CR3")
my_cr4 <- my_data %>%
  filter(grepl("CR4", my_data$Comment)) %>%
  mutate("RockName" = "CR4")


my_data <- rbind(my_cr3, my_cr4)
my_data <- my_data[,c(29, 23, 2:15)]
my_data <- my_data %>% 
  mutate("MgN" = ((MgO / (40.31)) / ((MgO / (40.31)) + (FeO / (71.85)))) * (100))
my_data$RockName <- as.factor(my_data$RockName)


my_data_wt <- my_data %>%
  select("RockName":"Total", "MgN") %>%  #choose relevant columns
  filter(Total > 98.0 & Total < 101.0) %>%
  filter(SiO2 > 40.0)
my_data_wt <- my_data_wt[1:10, 1:5]
my_data_wt

## now try rearrange data
my_avg_m <- my_data_wt %>%
  group_by(RockName) %>%
  summarise(n = n(), "SiO2" = mean(SiO2), "TiO2" = mean(TiO2), .groups = "keep")
my_avg_m2 <- as.data.frame(my_avg_m)
my_avg_m3 <- my_avg_m2[,-1]
rownames(my_avg_m3) <- my_avg_m2[, 1]
my_avg_mtran <- transpose(my_avg_m3)
rownames(my_avg_mtran) <- colnames(my_avg_m3)
colnames(my_avg_mtran) <- rownames(my_avg_m3)
rownames_to_column(my_avg_mtran, var = "Element")
