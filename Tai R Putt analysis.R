library(tidyverse)
library(reshape2)
library(dplyr)

# Read data from file
data <- read.csv("C:/Users/chikazhet/waikato.csv")

#Inspect the data
#Inspect the data
str(data)
# filtering data based on season
temp1 <- filter(data, season == "2021_22")

temp2 <- filter(temp1, district == "South Waikato")



# Filtering based on profit above or below median 2021/22 season
LowOP <- filter(temp1, Operating_profit_ha <= median(temp1$Operating_profit_ha))
HighOP <- filter(temp1, Operating_profit_ha > median(temp1$Operating_profit_ha))

#binding the filtered data and creating a Profitability column
ding <-  rbind(HighOP %>% mutate(Profitability = "A"), 
LowOP %>% mutate(Profitability = "B"))


#Selecting the variables we need
ding2 <- select(ding,season,Farm,Operating_profit_ha,Farm_working_expenses_per_kgMS,MS_per_ha,methane,total_feed_eaten_tha,pasture_and_crop_eaten_per_ha,N_surplus,N_fertiliser_per_ha,imported_supplements_eaten_tha, Profitability, milking_interval)

# Presenting the data into a long format
molten.data <- melt(ding2, id = c("Profitability","milking_interval", "season","Farm"))




# Plotting data on multiple-variables 2021-22
p2 <- ggplot(molten.data, aes(x=Profitability, y = value, fill=Profitability, label=Farm)) + 
  geom_boxplot() +
  theme_gray() +
  #geom_jitter() +
  geom_text(aes(label=ifelse(Farm=="PF",as.character(Farm),'')),hjust=0,vjust=0,fontface="bold", size=2) +
  theme(legend.position = "none")+
  facet_wrap(~variable,scales = "free",labeller = as_labeller(
    c(Operating_profit_ha = "Operating profit $/ha", Farm_working_expenses_per_kgMS = "Farm working expenses $/kgMS",MS_per_ha ="Milksolids kg/ha", methane ="Methane t CO2 equiv/ha",total_feed_eaten_tha = "Total feed eaten t DM/ha",pasture_and_crop_eaten_per_ha="Pasture and crop eaten t DM/ha", N_surplus="N surplus kg N/ha",N_fertiliser_per_ha = "N fertiliser kg N/ha", imported_supplements_eaten_tha = "Imported supplements eaten t DM/ha" ))) +
  labs(x = NULL, y = NULL) +
  labs(title="2021-22 season(Waikato Region-123 farms) A= Operating profit above, B= below median") +
  theme(axis.title.x = element_text(face="bold", size=10),axis.text.x  = element_text(size=8)) +
  theme(axis.title.y = element_text(face="bold", size=10),axis.text.y  = element_text(size=8)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5)) 
p2



# Filtering based on profit above or below median 2021/22 season South Waikato district
LowOP <- filter(temp2, Operating_profit_ha <= median(temp1$Operating_profit_ha))
HighOP <- filter(temp2, Operating_profit_ha > median(temp1$Operating_profit_ha))

#binding the filtered data and creating a Profitability column
ding <-  rbind(HighOP %>% mutate(Profitability = "A"), 
               LowOP %>% mutate(Profitability = "B"))


#Selecting the variables we need
ding2 <- select(ding,season,Farm,Operating_profit_ha,Farm_working_expenses_per_kgMS,MS_per_ha,methane,total_feed_eaten_tha,pasture_and_crop_eaten_per_ha,N_surplus,N_fertiliser_per_ha,imported_supplements_eaten_tha, Profitability, milking_interval)

# Presenting the data into a long format
molten.data <- melt(ding2, id = c("Profitability","milking_interval", "season","Farm"))




# Plotting data on multiple-variables 2021-22
p2 <- ggplot(molten.data, aes(x=Profitability, y = value, fill=Profitability, label=Farm)) + 
  geom_boxplot() +
  theme_gray() +
  #geom_jitter() +
  geom_text(aes(label=ifelse(Farm=="PF",as.character(Farm),'')),hjust=0,vjust=0,fontface="bold", size=2) +
  theme(legend.position = "none")+
  facet_wrap(~variable,scales = "free",labeller = as_labeller(
    c(Operating_profit_ha = "Operating profit $/ha", Farm_working_expenses_per_kgMS = "Farm working expenses $/kgMS",MS_per_ha ="Milksolids kg/ha", methane ="Methane t CO2 equiv/ha",total_feed_eaten_tha = "Total feed eaten t DM/ha",pasture_and_crop_eaten_per_ha="Pasture and crop eaten t DM/ha", N_surplus="N surplus kg N/ha",N_fertiliser_per_ha = "N fertiliser kg N/ha", imported_supplements_eaten_tha = "Imported supplements eaten t DM/ha" ))) +
  labs(x = NULL, y = NULL) +
  labs(title="2021-22 season(South Waikato-22 farms) A= Operating profit above, B= below median") +
  theme(axis.title.x = element_text(face="bold", size=10),axis.text.x  = element_text(size=8)) +
  theme(axis.title.y = element_text(face="bold", size=10),axis.text.y  = element_text(size=8)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5)) 
p2




