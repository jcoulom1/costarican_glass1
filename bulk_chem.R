#####
#Bulk Chemistry Analysis#
#####


#Load packages and data
library(dbplyr)
library(tidyverse)
library(gridExtra)
bulk_data <- read.csv("C:/Users/labry/Documents/R/costarican_glass1/data/bulk_comp_data.csv")

#Create new columns for RockName
bulk_data$Comment <- as.character(bulk_data$Comment)
blk_cr1a <- bulk_data %>%
  filter(grepl("CR1A", bulk_data$Comment)) %>%
  mutate("RockName" = "CR1A")
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
blk_data <- rbind(blk_cr1a, blk_cr1b, blk_cr3, blk_cr6, blk_cr7)
blk_data <- blk_data[,c(29, 23, 2:15)]
