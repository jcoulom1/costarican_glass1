#####
##Streamline data filtering from CSV
#####

##Load data and packages

test_data <- read.csv("C:/Users/labry/Documents/R/costarican_glass1/data/ProbeData_Test.csv")
library(dbplyr)
library(tidyverse)

test_data[test_data == ""] <- NA

test_data_wt <- test_data %>%
  select("Comment", "P2O5":"K2O", "Total") %>%  #choose relevant columns
  filter(Total > 95.0 & Total < 101.0) %>% #select rows based on Total
  filter(!is.na(P2O5) & SiO2 < 90.0 & Al2O3 < 22.0 & K2O > 1.0) #select rows based on elements




