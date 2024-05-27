
#Load the necessary libraries
library(tidyverse)
library(ggplot2)
library("reshape2")
library("ggplot2")
library(KernSmooth)
library(dplyr)

df<-read.csv("C:/Users/chikazhet/lownselwyn.csv")

# All mitigations plotted
MyData2<- df


#plotting profit and N leaching
ggplot(data = MyData2, mapping = aes(y = profit_change, x = leaching_change, group= Farmid))+
scale_x_reverse(position="top")+
  geom_path(aes(color= Farmid),size=1)+ 
  scale_colour_manual(values = c("green","black","red","blue"))+
  geom_point(aes(shape=mitigation), size=2)+scale_shape_manual(values=c("Base"=10, "lessN"=16,"Italian"=7, "Plantain"=15, "pasture_baleage"= 17))+ 
  labs(y="Change in operating profit(%)", x="Change in N leaching(%)") 


##plotting profit and GHG
  ggplot(data = MyData2, mapping = aes(y = profit_change, x = ghg_change, group= Farmid))+
  scale_x_reverse(position="top")+
  geom_path(aes(color= Farmid),size=1)+ 
    scale_colour_manual(values = c("green","black","red","blue"))+
  geom_point(aes(shape=mitigation), size=2)+scale_shape_manual(values=c("Base"=10, "lessN"=16,"Italian"=7, "Plantain"=15, "pasture_baleage"= 17))+  
  labs(y="Change in operating profit(%)", x="Change in GHG(%)") 


  
  # $/ha loss per % change in N leaching
  MyData2$mitigation <- factor(MyData2$mitigation, levels= c("Base", "lessN","Italian", "Plantain", "pasture_baleage"))
  ## plotting profit and N leaching
  ggplot(data =MyData2, mapping = aes(y =dollar_change_nloss, x = N_loss, group= mitigation, color=mitigation)) +
    scale_x_reverse(position="top") +
    geom_point(aes(color= mitigation),size=2)+ scale_colour_manual(values = c("grey","green","black","red","blue" ))+
    scale_fill_discrete(limits = c("Base", "lessN","Italian", "Plantain", "pasture_baleage")) +
    labs(y="Mitigation cost($/ha)", x="Mitigation N leaching reduction(%)") + 
    theme(legend.title=element_blank()) 
  
  
  
  