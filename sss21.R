#setting up
setwd("~/Documents/Rprojects/student_satisfaction_survey/data")
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
sss21<-read_excel("2021 SSS Raw.xlsx")
names(sss21)
#glimpse(sss21$`Invite Custom Field 1`)

#select (and rename) variables to build separate data frames
sss21.qual<-select(sss21, starts_with("Please explain"))
names(sss21.qual)

sss21.slct<-sss21 %>% select(contains(":Please rate")) 
#names(sss21.slct) [1]<-"Pid"
names(sss21.slct)<- gsub(":[A-Za-z]*.*$", "", names(sss21.slct))
names(sss21.slct)

#revert to data frame to make variables and detect values for each variable
df_sss21<-as.data.frame(sss21.slct)
str(df_sss21)

#--------------------------------------------------------
#go through each item [1-24]
#[1] Overall facilities
#1.as factor
df_sss21 <- df_sss21 %>% mutate (facilities_fac = factor(df_sss21$`Overall facilities (i.e. academic, residential and recreational spaces)`, levels = c('Poor', 'Acceptable', 'Good', 'Excellent','NA')) )
df_sss21$facilities_fac #好像的确没有na/ no usage的选项？
#2.pivot table
facilities21_tab <- df_sss21 %>% 
  filter(!is.na(facilities_fac)) %>% 
  group_by(facilities_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#1.0f%%" )) #'%#.2f'
facilities21_tab
#3.bar plot
facilities21_tab %>% 
  ggplot(aes(x =facilities_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = "Facilities (i.e. academic, residential and recreational spaces) Satisfaction (2021)")+
  theme(plot.title = element_text(color = "#003057", size = 12, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  geom_text(aes(label = cnt), vjust = 2, size = 3, color="#ffffff") #vjust=-0.5 lable up

#-----------------------------------------------------------------
# merge with data mart and fall enrollment
# read datamart and fall enrollment data
datamart21<- as.data.frame(read_excel("Datamart21Fall.xlsx"))
str(datamart21)
registar21<- as.data.frame(read_excel("Registar21Fall.xlsx"))
str(registar21)

left_join(df_sss21,datamart21,by=)

