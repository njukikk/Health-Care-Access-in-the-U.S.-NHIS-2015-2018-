#Kelvin Njuki
#STA 504 - Section A
#Advanced Data Visualization
#Solo Project
#4/6/2020

#===============================================================================================#
#===============================================================================================#

#Setting working directory
setwd("//tsclient/G/My Drive/Miami/Masters/Spring2020/STA 504/Project/Final")

#Loading tidyverse package
library(tidyverse)

#Reading in datasets from 2015 - 2018
family2015 <- read_csv("familyxx2015.csv")
family2016 <- read_csv("familyxx2016.csv")
family2017 <- read_csv("familyxx2017.csv")
family2018 <- read_csv("familyxx2018.csv")

#Combining the above four datasets
family_combined <- rbind(family2015, family2016, family2017, family2018)

#Data cleaning (Removing missing values, selecting variables of interest 
#and adding labels to factor levels)
family_combined_clean <- family_combined %>%
  select(SRVY_YR,FNMEDYN,FHICOST,FMEDBILL,FHICOVYN,FHICOVCT) %>%
  filter(SRVY_YR!=9998,FNMEDYN!=7 | FNMEDYN!=8 | FNMEDYN!=9,FHICOST!=7 | FHICOST!=8 | FHICOST!=9,
         FMEDBILL!=7 | FMEDBILL!=8 | FMEDBILL!=9,FHICOVYN!=7 | FHICOVYN!=8 | FHICOVYN!=9) %>%
  mutate(FNMEDYN=factor(FNMEDYN,levels = c(1,2),labels = c("Yes","No"))) %>%
  mutate(FHICOST=factor(FHICOST,levels = c(0,1,2,3,4,5),
                        labels = c("$0","Less than $500","$500-$1,999","$2,000-$2,999",
                                   "$3,000-$4,999","$5,000 or more"))) %>%
  mutate(FMEDBILL=factor(FMEDBILL,levels = c(1,2),labels = c("Yes","No"))) %>%
  mutate(FHICOVYN=factor(FHICOVYN,levels = c(1,2),labels = c("Yes","No"))) %>%
  na.omit

#===============================================================================================#
#===============================================================================================#

#----------------------------------------GENERATING PLOTS---------------------------------------#

#Distribution of number of family members with health care coverage

family_combined_clean %>% 
  count(FHICOVCT) %>% 
  mutate(Percentage = round(prop.table(n),2)) %>% 
  ggplot(aes(x = FHICOVCT, y = Percentage, label = scales::percent(Percentage))) + 
  geom_col(fill="orange") + 
  geom_text(vjust = -0.5, size = 3) + 
  labs(x="Number of family members with health care coverage",
       title="Distribution of number of family members with 
       health care coverage",
       subtitle="From 2015 - 2018",
       caption ="Data Source: NCHS, National Health Interview Survey, 2015 - 2018") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5))

#===============================================================================================#
#Number of family members with health care coverage across the four years

ggplot()+
  geom_bar(aes(x=SRVY_YR, fill=FHICOVCT),fill="orange",stat="count",position ="dodge",
           data=family_combined_clean) +
  geom_text(aes(x=SRVY_YR,label=..count..),data=family_combined_clean,stat="count",vjust=-0.5,
            size=3) +
  labs(x="Survey Year",y="Totals",
       title="Number of family members with health care coverage",
       subtitle="From 2015 - 2018",
       caption ="Data Source: NCHS, National Health Interview Survey, 2015 - 2018") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("one.png",dpi=600,height=4,width=5, units="in")
#===============================================================================================#

#A bar chart of medical care affordability from 2015 - 2018

ggplot()+
  geom_bar(aes(x=SRVY_YR,fill=FNMEDYN), stat="count",position ="dodge",
           data=family_combined_clean) + 
  scale_fill_manual(values=c("grey","orange"),name="Unable to afford 
medical care") +
  labs(x="Survey Year", y="Totals",
       title="Medical care affordability", 
       subtitle="From 2015 - 2018",
       caption ="Data Source: NCHS, National Health Interview
Survey, 2015 - 2018") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust=1))
ggsave("two.png",dpi=600,height=4,width=7, units="in")
#===============================================================================================#

#Heatmap of Could not afford medical care and having a family member with a 
#health care coverage

#Preparing the dataset to be used in generating heatmap
family2018_clean_heatmap <- family_combined_clean %>%
  group_by(FNMEDYN, FHICOVYN,SRVY_YR) %>%
  summarise(n=n())

ggplot() +
  geom_tile(aes(x=FNMEDYN,y=FHICOVYN,fill=n),data=family2018_clean_heatmap) + 
  scale_fill_gradient(low="white",high="orange") +
  labs(x="Could not afford medical care",y="Any family member has health care coverage",
       fill = "Frequency",title="Heatmap of unable to afford medical care and 
       having health care coverage",
       subtitle="From 2015 - 2018",
       caption ="Data Source: NCHS, National Health Interview Survey, 2015 - 2018") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  facet_grid(SRVY_YR~.)
ggsave("three.png",dpi=600,height=4,width=5, units="in")
#===============================================================================================#

#Correlation between health care cost and having any family member has health care coverage

#Loading ggmosaic package
library(ggmosaic)
ggplot() +
  geom_mosaic(aes(x=product(FHICOVYN), fill=FHICOST), data=family_combined_clean) +
  labs(x="Any family member has health care coverage",y="Health Care Cost",
       title = "Mosaic plot of health care cost and having 
       health care coverage", 
       subtitle="From 2015 - 2018",
       caption ="Data Source: NCHS, National Health Interview Survey, 2015 - 2018") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5),legend.position ="none")
ggsave("four.png",dpi=600,height=4,width=5, units="in")
#===============================================================================================#

