# Create a Population Pyramid for >=50 years 
# This code creates a pyramid plot using ggplot2

# Written By Wafula Erick
# Date: 15-03-2020 (dd/mm/yyyy) 

# Load the required libraries
library(plyr)
library(dplyr)
library(ggplot2)
#Summarize the data

d<-mydata_df %>%
  filter(!is.na(sex_0)) %>%
  group_by(age_cat3,sex_0) %>%
  summarise(total_pop=n()) %>%
  mutate(total_pop=ifelse(sex_0=="Male",-total_pop,total_pop)) #This is done so that the males and females can appear on the different sides of the pyramid

g1 <- ggplot(d, aes(x = age_cat3, y = total_pop, fill = sex_0)) + 
  geom_bar(data = subset(d, sex_0 == "Female"), stat = "identity") + 
  geom_bar(data = subset(d, sex_0 == "Male"), stat = "identity") + 
  scale_y_continuous(labels = abs, limits = max(d$total_pop) * c(-1,1)) +
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()+
  #theme(legend.position="top") +
  labs(y="Population", title = "Polulation Pyramid for SEARCH Participants, >=50 Years n=35,275",
       x="Age Category")

g1