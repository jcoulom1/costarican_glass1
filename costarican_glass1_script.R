#####
##Begin Costa Rica Glass project
#####

##Load data and packages
cr_data <- read.csv("data/costa_rican_data2.csv")
library(dbplyr)
library(tidyverse)

#create new column to sort by bulk rocks
rock_cr1a <- cr_data %>%
   filter(grepl("CR1A", cr_data$Name)) %>%
  mutate("RockName" = "CR1A")
rock_cr1b <- cr_data %>%
  filter(grepl("CR1B", cr_data$Name)) %>%
  mutate("RockName" = "CR1B")
rock_cr2a <- cr_data %>%
  filter(grepl("CR2A", cr_data$Name)) %>%
  mutate("RockName" = "CR2A")
rock_cr2b <- cr_data %>%
  filter(grepl("CR2B", cr_data$Name)) %>%
  mutate("RockName" = "CR2B")
rock_cr3 <- cr_data %>%
  filter(grepl("CR3", cr_data$Name)) %>%
  mutate("RockName" = "CR3")
rock_cr4 <- cr_data %>%
  filter(grepl("CR4", cr_data$Name)) %>%
  mutate("RockName" = "CR4")
rock_cr5 <- cr_data %>%
  filter(grepl("CR5", cr_data$Name)) %>%
  mutate("RockName" = "CR5")
rock_data <- rbind(rock_cr1a, rock_cr1b, rock_cr2a, rock_cr2b, rock_cr3, rock_cr4, rock_cr5)
rock_data <- rock_data[,c(17, 2:16)]
