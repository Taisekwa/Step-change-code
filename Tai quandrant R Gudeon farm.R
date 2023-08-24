library(tidyverse)
library(reshape2)
library(dplyr)

## Read data from file
data <- read.csv("C:/Users/chikazhet/gudgeon.csv")

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


#Construct the quadrant graphs for 2021/22 season

ggplot(data2, aes(N_surplus,Operating_profit_ha, label=Farm)) +
  
  geom_point(size=1.5, color="red") +
  
  theme_gray() +
  
  geom_hline(yintercept=median(data$Operating_profit_ha),
                                                
 col="blue") + geom_vline(xintercept=median(data$N_surplus), 
                        
  col="blue") + annotate("text", x = 0, y = 7500, label = "A", col = "blue", size = 10) +
  
  annotate("text", x = 250, y = 7500, label = "B", col = "blue", size = 10)+
  
  annotate("text", x = 0, y = 1250, label = "C", col = "blue", size = 10) +
  
  annotate("text", x = 250, y = 1250, label = "D", col = "blue", size = 10) +
  
  #annotate("text", x = -35, y = 4512, label = "Gudgeon", col = "black", size = 4, fontface="bold") +
  
  geom_text(aes(label=ifelse(Farm=="GF",as.character(Farm),'')),hjust=0,vjust=0,fontface="bold") +

  xlab("Purchased N surplus (kg N/ha/yr)") + ylab("Dairy operating profit ($/ha)") +
  
  labs(title="Operating profit vs purchased N surplus 2021-22 season") + 
  
  theme(plot.title=element_text(hjust=0.5)) + 
  
  theme(legend.position = "bottom") +
  
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=8)) +
  
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=8)) +
  
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5))


# methane quandrant graph with AAA, farm 1 and 2 positions.

ggplot(data2, aes(methane,Operating_profit_ha,label=Farm)) +
  
  geom_point(size=1.5, col="red") +
  
  theme_gray() +
  
  geom_hline(yintercept=median(data$Operating_profit_ha), col="blue") + 
  
  geom_vline(xintercept=median(data$methane), col="blue")+
  
  annotate("text", x = 5, y = 7500, label = "A", col = "blue", size = 10) +
  
  annotate("text", x = 12.5, y = 7500, label = "B", col = "blue", size = 10)+
  
  annotate("text", x = 5, y = 1250, label = "C", col = "blue", size = 10) +
  
  annotate("text", x = 12.5, y = 1250, label = "D", col = "blue", size = 10) +
  
  #annotate("text", x = 8.8, y = 4512, label = "Gudgeon", col = "black", size = 4, fontface="bold") +
  
  geom_text(aes(label=ifelse(Farm=="GF",as.character(Farm),'')),hjust=0,vjust=0,fontface="bold" ) +
  
  xlab("Methane emission (t CO2 equiv./ha/yr)") + 
  
  ylab("Dairy operating profit ($/ha)") + 
  
  labs(title="Operating profit vs methane emissions 2021-22 season") + 
  
  theme(plot.title=element_text(hjust=0.5)) + 
  
  theme(legend.position = "bottom") +
  
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=8)) +
  
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=8)) +
  
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5))


# Filter 2020/21 season and subset profitability based on below or above median profit.

data3 <- data %>% filter(data$season== "2020_21")%>%
  dplyr::mutate(Profitability = 
                  dplyr::case_when(
                    Operating_profit_ha > median(Operating_profit_ha) ~ "Above median",
                    Operating_profit_ha <= median(Operating_profit_ha) ~ "Below median"))
profit_drivers <- ggplot(data3, aes(x = methane, y = Operating_profit_ha, color=Profitability)) +
  geom_point()
profit_drivers
data3$season<-  factor(data3$season)


MedianOperating_profit_ha <- median(data3$Operating_profit_ha)

#Construct the quadrant graphs for 2020/21 season

ggplot(data3, aes(N_surplus,Operating_profit_ha, label=Farm)) +
  
  geom_point(size=1.5, color="red") +
  
  theme_gray() +
  
  geom_hline(yintercept=median(data3$Operating_profit_ha),
             
             col="blue") + geom_vline(xintercept=median(data3$N_surplus), 
                                      
                                      col="blue") + annotate("text", x = 0, y = 7500, label = "A", col = "blue", size = 10) +
  
  annotate("text", x = 250, y = 7500, label = "B", col = "blue", size = 10)+
  
  annotate("text", x = 0, y = 1250, label = "C", col = "blue", size = 10) +
  
  annotate("text", x = 250, y = 1250, label = "D", col = "blue", size = 10) +
  
  #annotate("text", x = 45, y = 2561, label = "Gudgeon", col = "black", size = 4, fontface="bold") +
  
  geom_text(aes(label=ifelse(Farm=="GF",as.character(Farm),'')),hjust=0,vjust=0, fontface="bold") +
  
  xlab("Purchased N surplus (kg N/ha/yr)") + ylab("Dairy operating profit ($/ha)") +
  
  labs(title="Operating profit vs purchased N surplus 2020-21 season") + 
  
  theme(plot.title=element_text(hjust=0.5)) + 
  
  theme(legend.position = "bottom") +
  
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=8)) +
  
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=8)) +
  
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5))


# methane quandrant graph with AAA.

ggplot(data3, aes(methane,Operating_profit_ha,label=Farm)) +
  
  geom_point(size=1.5, col="red") +
  
  theme_gray() +
  
  geom_hline(yintercept=median(data3$Operating_profit_ha), col="blue") + 
  
  geom_vline(xintercept=median(data3$methane), col="blue")+
  
  annotate("text", x = 5, y = 7500, label = "A", col = "blue", size = 10) +
  
  annotate("text", x = 12.5, y = 7500, label = "B", col = "blue", size = 10)+
  
  annotate("text", x = 5, y = 1250, label = "C", col = "blue", size = 10) +
  
  annotate("text", x = 12.5, y = 1250, label = "D", col = "blue", size = 10) +
  
  #annotate("text", x = 8.8, y = 2561, label = "Gudgeon", col = "black", size = 4, fontface="bold") +
  
  geom_text(aes(label=ifelse(Farm=="GF",as.character(Farm),'')),hjust=0,vjust=0,fontface="bold" ) +
  
  xlab("Methane emission (t CO2 equiv./ha/yr)") + 
  
  ylab("Dairy operating profit ($/ha)") + 
  
  labs(title="Operating profit vs methane emissions 2020-21 season") + 
  
  theme(plot.title=element_text(hjust=0.5)) + 
  
  theme(legend.position = "bottom") +
  
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=8)) +
  
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=8)) +
  
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5))


# Filter 2019/20 season and subset profitability based on below or above median profit.

data4 <- data %>% filter(data$season== "2019_20")%>%
  dplyr::mutate(Profitability = 
                  dplyr::case_when(
                    Operating_profit_ha > median(Operating_profit_ha) ~ "Above median",
                    Operating_profit_ha <= median(Operating_profit_ha) ~ "Below median"))
profit_drivers <- ggplot(data4, aes(x = methane, y = Operating_profit_ha, color=Profitability)) +
  geom_point()
profit_drivers
data4$season<-  factor(data4$season)


MedianOperating_profit_ha <- median(data4$Operating_profit_ha)

#Construct the quadrant graphs for 2019/20 season

ggplot(data4, aes(N_surplus,Operating_profit_ha,label=Farm)) +
  
  geom_point(size=1.5, color="red") +
  
  theme_gray() +
  
  geom_hline(yintercept=median(data4$Operating_profit_ha),
             
             col="blue") + geom_vline(xintercept=median(data4$N_surplus), 
                                      
                                      col="blue") + annotate("text", x = 0, y = 7500, label = "A", col = "blue", size = 10) +
  
  annotate("text", x = 250, y = 7500, label = "B", col = "blue", size = 10)+
  
  annotate("text", x = 0, y = -4000, label = "C", col = "blue", size = 10) +
  
  annotate("text", x = 250, y = -4000, label = "D", col = "blue", size = 10) +
  
  #annotate("text", x = 67, y = 2242, label = "Gudgeon", col = "black", size = 4, fontface="bold") +
  
  geom_text(aes(label=ifelse(Farm=="GF",as.character(Farm),'')),hjust=0,vjust=0,fontface="bold" ) +
  
  xlab("Purchased N surplus (kg N/ha/yr)") + ylab("Dairy operating profit ($/ha)") +
  
  labs(title="Operating profit vs purchased N surplus 2019-20 season") + 
  
  theme(plot.title=element_text(hjust=0.5)) + 
  
  theme(legend.position = "bottom") +
  
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=8)) +
  
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=8)) +
  
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5))


# methane quandrant graph with AAA.

ggplot(data4, aes(methane,Operating_profit_ha, label=Farm)) +
  
  geom_point(size=1.5, col="red") +
  
  theme_gray() +
  
  geom_hline(yintercept=median(data4$Operating_profit_ha), col="blue") + 
  
  geom_vline(xintercept=median(data4$methane), col="blue")+
  
  annotate("text", x = 5, y = 7500, label = "A", col = "blue", size = 10) +
  
  annotate("text", x = 12.5, y = 7500, label = "B", col = "blue", size = 10)+
  
  annotate("text", x = 5, y = -4000, label = "C", col = "blue", size = 10) +
  
  annotate("text", x = 12.5, y = -4000, label = "D", col = "blue", size = 10) +
  
  #annotate("text", x = 8.5, y = 2242, label = "Gudgeon", col = "black", size = 4, fontface="bold") +
  
  geom_text(aes(label=ifelse(Farm=="GF",as.character(Farm),'')),hjust=0,vjust=0, fontface="bold") +
  
  xlab("Methane emission (t CO2 equiv./ha/yr)") + 
  
  ylab("Dairy operating profit ($/ha)") + 
  
  labs(title="Operating profit vs methane emissions 2019-20 season") + 
  
  theme(plot.title=element_text(hjust=0.5)) + 
  
  theme(legend.position = "bottom") +
  
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=8)) +
  
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=8)) +
  
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5))



