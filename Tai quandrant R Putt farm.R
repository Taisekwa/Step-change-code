library(tidyverse)
library(reshape2)
library(dplyr)

## Read data from file
data <- read.csv("C:/Users/chikazhet/waikato.csv")

##Inspect the data
##Inspect the data
str(data)

# Filter 2021/22 season and subset profitability based on below or above median profit.

data2 <- data %>% filter(data$season== "2021_22")%>%
  dplyr::mutate(Profitability = 
                  dplyr::case_when(
                    Operating_profit_ha > median(Operating_profit_ha) ~ "Above median",
                    Operating_profit_ha <= median(Operating_profit_ha) ~ "Below median"))
profit_drivers <- ggplot(data2, aes(x = methane, y = Operating_profit_ha, color=Profitability)) +
  geom_point()
profit_drivers
data2$season<-  factor(data2$season)


MedianOperating_profit_ha <- median(data2$Operating_profit_ha)


#Construct the quadrant graphs for 2021/22 season Waikato

ggplot(data2, aes(N_surplus,Operating_profit_ha, label=Farm)) +
  
  geom_point(size=1.5, color="red") +
  
  geom_hline(yintercept=median(data$Operating_profit_ha),
                                                
 col="blue") + geom_vline(xintercept=median(data$N_surplus), 
                        
  col="blue") + annotate("text", x = 0, y = 7500, label = "A", col = "blue", size = 10) +
  
  annotate("text", x = 250, y = 7500, label = "B", col = "blue", size = 10)+
  
  annotate("text", x = 0, y = 1250, label = "C", col = "blue", size = 10) +
  
  annotate("text", x = 250, y = 1250, label = "D", col = "blue", size = 10) +
  
   geom_text(aes(label=ifelse(Farm=="PF",as.character(Farm),'')),hjust=0,vjust=0,fontface="bold") +

  xlab("Purchased N surplus (kg N/ha/yr)") + ylab("Dairy operating profit ($/ha)") +
  
  labs(title="Operating profit vs purchased N surplus 2021-22 season") + 
  
  theme(plot.title=element_text(hjust=0.5)) + 
  
  theme(legend.position = "bottom") +
  
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=8)) +
  
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=8)) +
  
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5))


# methane quandrant graph with AAA, farm 1 and 2 positions 2021/22 Waikato.

ggplot(data2, aes(methane,Operating_profit_ha,label=Farm)) +
  
  geom_point(size=1.5, col="red") +
  
  geom_hline(yintercept=median(data$Operating_profit_ha), col="blue") + 
  
  geom_vline(xintercept=median(data$methane), col="blue")+
  
  annotate("text", x = 5, y = 7500, label = "A", col = "blue", size = 10) +
  
  annotate("text", x = 12.5, y = 7500, label = "B", col = "blue", size = 10)+
  
  annotate("text", x = 5, y = 1250, label = "C", col = "blue", size = 10) +
  
  annotate("text", x = 12.5, y = 1250, label = "D", col = "blue", size = 10) +
  
  #annotate("text", x = 8.8, y = 4512, label = "Gudgeon", col = "black", size = 4, fontface="bold") +
  
  geom_text(aes(label=ifelse(Farm=="PF",as.character(Farm),'')),hjust=0,vjust=0,fontface="bold" ) +
  
  xlab("Methane emission (t CO2 equiv./ha/yr)") + 
  
  ylab("Dairy operating profit ($/ha)") + 
  
  labs(title="Operating profit vs methane emissions 2021-22 season") + 
  
  theme(plot.title=element_text(hjust=0.5)) + 
  
  theme(legend.position = "bottom") +
  
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=8)) +
  
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=8)) +
  
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5))


#Quadrant graph for the South Waikato 2021/22

tai <- data2 %>% filter(data2$district== "South Waikato") %>%
  dplyr::mutate(Profitability = 
                  dplyr::case_when(
                    Operating_profit_ha > median(Operating_profit_ha) ~ "Above median",
                    Operating_profit_ha <= median(Operating_profit_ha) ~ "Below median"))
profit_drivers <- ggplot(datatai, aes(x = methane, y = Operating_profit_ha, color=Profitability)) +
  geom_point()
profit_drivers
data$season<-  factor(data$season)


MedianOperating_profit_ha <- median(tai$Operating_profit_ha)


#Construct the quadrant graphs for  south Waikato 2021/22 season

ggplot(tai, aes(N_surplus,Operating_profit_ha, label=Farm)) +
  
  geom_point(size=1.5, color="red") +
  
  geom_hline(yintercept=median(data$Operating_profit_ha),
             
             col="blue") + geom_vline(xintercept=median(data$N_surplus), 
                                      
                                      col="blue") + annotate("text", x = 0, y = 7500, label = "A", col = "blue", size = 10) +
  
  annotate("text", x = 250, y = 7500, label = "B", col = "blue", size = 10)+
  
  annotate("text", x = 0, y = 1250, label = "C", col = "blue", size = 10) +
  
  annotate("text", x = 250, y = 1250, label = "D", col = "blue", size = 10) +
  
  #annotate("text", x = -35, y = 4512, label = "Gudgeon", col = "black", size = 4, fontface="bold") +
  
  geom_text(aes(label=ifelse(Farm=="PF",as.character(Farm),'')),hjust=0,vjust=0,fontface="bold") +
  
  xlab("Purchased N surplus (kg N/ha/yr)") + ylab("Dairy operating profit ($/ha)") +
  
  labs(title="Operating profit vs purchased N surplus 2021-22 season") + 
  
  theme(plot.title=element_text(hjust=0.5)) + 
  
  theme(legend.position = "bottom") +
  
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=8)) +
  
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=8)) +
  
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5))


# South Waikato 2021/22 methane quandrant graph with AAA, farm 1 and 2 positions.

ggplot(tai, aes(methane,Operating_profit_ha,label=Farm)) +
  
  geom_point(size=1.5, col="red") +
  
  geom_hline(yintercept=median(data$Operating_profit_ha), col="blue") + 
  
  geom_vline(xintercept=median(data$methane), col="blue")+
  
  annotate("text", x = 5, y = 7500, label = "A", col = "blue", size = 10) +
  
  annotate("text", x = 12.5, y = 7500, label = "B", col = "blue", size = 10)+
  
  annotate("text", x = 5, y = 1250, label = "C", col = "blue", size = 10) +
  
  annotate("text", x = 12.5, y = 1250, label = "D", col = "blue", size = 10) +
  
  #annotate("text", x = 8.8, y = 4512, label = "Gudgeon", col = "black", size = 4, fontface="bold") +
  
  geom_text(aes(label=ifelse(Farm=="PF",as.character(Farm),'')),hjust=0,vjust=0,fontface="bold" ) +
  
  xlab("Methane emission (t CO2 equiv./ha/yr)") + 
  
  ylab("Dairy operating profit ($/ha)") + 
  
  labs(title="Operating profit vs methane emissions 2021-22 season") + 
  
  theme(plot.title=element_text(hjust=0.5)) + 
  
  theme(legend.position = "bottom") +
  
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=8)) +
  
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=8)) +
  
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5))


# Filter 2022/23 season and subset profitability based on below or above median profit.

data3 <- data %>% filter(data$season== "2022_23")%>%
  dplyr::mutate(Profitability = 
                  dplyr::case_when(
                    Operating_profit_ha > median(Operating_profit_ha) ~ "Above median",
                    Operating_profit_ha <= median(Operating_profit_ha) ~ "Below median"))
profit_drivers <- ggplot(data3, aes(x = methane, y = Operating_profit_ha, color=Profitability)) +
  geom_point()
profit_drivers
data3$season<-  factor(data3$season)

profit_drivers1 <- ggplot(data3, aes(x = N_surplus, y = Operating_profit_ha, color=Profitability)) +
  geom_point()
profit_drivers1


MedianOperating_profit_ha <- median(data3$Operating_profit_ha)


#Construct the quadrant graphs for 2022/23 season Waikato

ggplot(data3, aes(N_surplus,Operating_profit_ha, label=Farm)) +
  
  geom_point(size=1.5, color="red") +
  
  geom_hline(yintercept=median(data$Operating_profit_ha),
             
             col="blue") + geom_vline(xintercept=median(data$N_surplus), 
                                      
                                      col="blue") + annotate("text", x = 0, y = 7500, label = "A", col = "blue", size = 10) +
  
  annotate("text", x = 250, y = 7500, label = "B", col = "blue", size = 10)+
  
  annotate("text", x = 0, y = 1250, label = "C", col = "blue", size = 10) +
  
  annotate("text", x = 250, y = 1250, label = "D", col = "blue", size = 10) +
  
  geom_text(aes(label=ifelse(Farm=="PF",as.character(Farm),'')),hjust=0,vjust=0,fontface="bold") +
  
  xlab("Purchased N surplus (kg N/ha/yr)") + ylab("Dairy operating profit ($/ha)") +
  
  labs(title="Operating profit vs purchased N surplus 2022-23 season") + 
  
  theme(plot.title=element_text(hjust=0.5)) + 
  
  theme(legend.position = "bottom") +
  
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=8)) +
  
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=8)) +
  
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5))


# methane quandrant graph with AAA, farm 1 and 2 positions 2022/23 Waikato.

ggplot(data3, aes(methane,Operating_profit_ha,label=Farm)) +
  
  geom_point(size=1.5, col="red") +
  
  geom_hline(yintercept=median(data$Operating_profit_ha), col="blue") + 
  
  geom_vline(xintercept=median(data$methane), col="blue")+
  
  annotate("text", x = 5, y = 7500, label = "A", col = "blue", size = 10) +
  
  annotate("text", x = 12.5, y = 7500, label = "B", col = "blue", size = 10)+
  
  annotate("text", x = 5, y = 1250, label = "C", col = "blue", size = 10) +
  
  annotate("text", x = 12.5, y = 1250, label = "D", col = "blue", size = 10) +
  
  #annotate("text", x = 8.8, y = 4512, label = "Gudgeon", col = "black", size = 4, fontface="bold") +
  
  geom_text(aes(label=ifelse(Farm=="PF",as.character(Farm),'')),hjust=0,vjust=0,fontface="bold" ) +
  
  xlab("Methane emission (t CO2 equiv./ha/yr)") + 
  
  ylab("Dairy operating profit ($/ha)") + 
  
  labs(title="Operating profit vs methane emissions 2022-23 season") + 
  
  theme(plot.title=element_text(hjust=0.5)) + 
  
  theme(legend.position = "bottom") +
  
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=8)) +
  
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=8)) +
  
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5))




