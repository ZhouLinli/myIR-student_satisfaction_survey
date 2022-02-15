#setting up
setwd("~/Documents/R projects/student_satisfaction_survey/data")
getwd()
list.files()
options("digits" = 4)   # two decimal

#load package
library(readxl) #for read_excel
library(readr) #for read_csv
library(dplyr) #for select, rename, join, etc.
library(tidyverse) #for pipes
library(ggplot2)
#-----------------------------------------------------------------------------
#read data files, initial exploration
sss17<-read_excel("2017SSSRaw.xlsx")
names(sss17)
glimpse(sss17$`Invite Custom Field 1`)

#select (and rename) variables to build separate data frames
    #~~sss17.qual<-select(sss17, starts_with("Please explain"))~~
    #~~names(sss17.qual)~~
sss17.slct<-sss17 %>% select(`Invite Custom Field 1`, contains(":Please rate your level of ")) 
names(sss17.slct) [1]<-"Pid"
names(sss17.slct)<- gsub(":[A-Za-z]*.*$", "", names(sss17.slct))
names(sss17.slct)

#revert to data frame to make variables and detect values for each variable
df_sss17<-as.data.frame(sss17.slct)
str(df_sss17)
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#read data files, initial exploration
sss18<-read_csv("2018SSSRaw.csv")
names(sss18)
#glimpse(sss18$`Invite Custom Field 1`)

#select (and rename) variables to build separate data frames
#~~sss18.qual<-select(sss18, starts_with("Please explain"))~~
#~~names(sss18.qual)~~
sss18.slct<-sss18 %>% select(contains(":Please rate")) 
#names(sss18.slct) [1]<-"Pid"
names(sss18.slct)<- gsub(":[A-Za-z]*.*$", "", names(sss18.slct))
names(sss18.slct)

#revert to data frame to make variables and detect values for each variable
df_sss18<-as.data.frame(sss18.slct)
str(df_sss18)

#--------------------------------
#go through each item
#Bookstore
#1.as factor
df_sss17$Bookstore = factor(df_sss17$Bookstore, levels = c('Poor', 'Acceptable', 'Good', 'Excellent','N/A')) 
is.factor(df_sss17$Bookstore)
#2.pivot table
Bookstore17<- df_sss17 %>% 
  filter(!is.na(`Bookstore`)) %>% 
  group_by(`Bookstore`) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#1.0f%%" )) #'%#.2f' 
#3.bar plot
Bookstore17 %>% 
  ggplot(aes(x = Bookstore, y = percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = "Bookstore Satisfaction (2017)")+
  theme(plot.title = element_text(color = "#003057", size = 12, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  geom_text(aes(label = cnt), vjust = 2, size = 3, color="#ffffff") #vjust=-0.5 lable up

#registar office
#go through each item
#1.as factor
df_sss17$`Registrar's Office` = factor(df_sss17$`Registrar's Office`, levels = c('Poor', 'Acceptable', 'Good', 'Excellent','N/A')) 
#2.pivot table
Registrar17<- df_sss17 %>% 
  filter(!is.na(`Registrar's Office`)) %>% 
  group_by(`Registrar's Office`) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#1.0f%%" )) #'%#.2f' 
#3.bar plot
Registrar17 %>% 
  ggplot(aes(x = `Registrar's Office`, y = percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = "Registrar's Office Satisfaction (2017)")+
  theme(plot.title = element_text(color = "#003057", size = 12, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  geom_text(aes(label = cnt), vjust = 2, size = 3, color="#ffffff") #vjust=-0.5 lable up


