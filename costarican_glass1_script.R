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

#Start to work on plots - SiO2 vs Na2O + K2O
alkali_plot <- ggplot(rock_data, aes(x = SiO2, y = Na2O + K2O)) +
  facet_wrap(vars(RockName)) + 
  geom_point() +
  geom_smooth()
alkali_plot


#create TAS plot to show data on
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
  annotate("text", label = "Basaltic \n trachy- \n andesite", x = 52.5, y = 7, size=4)+
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
