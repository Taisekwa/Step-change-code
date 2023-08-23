library(tidyverse)
library(reshape2)
library(dplyr)

# Read data from file
data <- read.csv("I:/Project-Archive/SLMACC West Coast/Observed Data/DairyBase data/west coast quadrant data for RTai.csv")

#Inspect the data
#Inspect the data
str(data)

# Filtering the data we need
LowOP <- filter(data, Operating_profit_ha <= median(data$Operating_profit_ha))
HighOP <- filter(data, Operating_profit_ha > median(data$Operating_profit_ha))

#binding the filtered data
temp <-  rbind(HighOP %>% mutate(Profitability = "Above median"), 
LowOP %>% mutate(Profitability = "Below median"))

#Selecting the variables we need
temp1 <- select(temp,Operating_profit_ha,Farm_working_expenses_per_kgMS,MS_per_ha,methane,total_feed_eaten_tha,pasture_and_crop_eaten_per_ha,N_surplus,N_fertiliser_per_ha,imported_supplements_eaten_tha, Profitability, milking_interval)

# Presenting the data into a long format
molten.data <- melt(temp1, id = c("Profitability","milking_interval"))

# Plotting data on multiple-variables
p2 <- ggplot(molten.data, aes(x=Profitability, y = value, fill=Profitability)) + 
  geom_boxplot() +
  geom_jitter() +
  theme(legend.position = "none")+
  facet_wrap(~variable,scales = "free",labeller = as_labeller(
    c(Operating_profit_ha = "Operating profit $/ha", Farm_working_expenses_per_kgMS = "Farm working expenses $/kgMS",MS_per_ha ="Milksolids kg/ha", methane ="Methane t CO2 equiv/ha",total_feed_eaten_tha = "Total feed eaten t DM/ha",pasture_and_crop_eaten_per_ha="Pasture and crop eaten t DM/ha", N_surplus="N surplus kg N/ha",N_fertiliser_per_ha = "N fertiliser kg N/ha", imported_supplements_eaten_tha = "Imported supplements eaten t DM/ha" ))) +
  labs(x = NULL, y = NULL) +
  theme(axis.title.x = element_text(face="bold", size=10),axis.text.x  = element_text(size=8)) +
  theme(axis.title.y = element_text(face="bold", size=10),axis.text.y  = element_text(size=8)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5)) 
p2







