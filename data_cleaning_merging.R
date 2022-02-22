#------------load package--------------
library(readxl) #for read_excel
library(readr) #for read_csv
library(dplyr) #for select, rename, join, etc.
library(tidyverse) #for pipes
library(ggplot2) #for making charts
library(stringr)  #for working with strings/texts/qualitative data
library("writexl")#for save data frame as excel files

#------working directory --------
getwd()
setwd("-/Documents/Rprojects/data/student_satisfaction_survey")


#============data files ===========

#------main data file--------------

#read data files, initial exploration
sss21<-read_excel("2021 SSS Raw.xlsx")
names(sss21)
#glimpse(sss21$`Invite Custom Field 1`)
#select (and rename) variables to build separate data frames
#

#create the main data file (qualitative)
sss21.qual<-select(sss21, starts_with("Please explain"), starts_with("Please provide explanations"), starts_with("Please provide any additional details"), names(sss21)[153])
names(sss21.qual)

#select variables to create the main quantitative data file
sss21.slct<-sss21 %>% select(`Invite Custom Field 1`,  `Which of the following describes your status for the 2021 fall semester?`, contains(":Please rate"), contains(":Â Please rate"), contains(":If it applies to you, please rate"), contains(":Â   Please estimate"), names(sss21)[138:152]) 
names(sss21.slct) [1]<-"pc_id"
names(sss21.slct)<- gsub(":[A-Za-z]*.*$", "", names(sss21.slct))
names(sss21.slct) [57]<-"Other_use_computer"
names(sss21.slct) [59]<-"Other_access_textbook"
names(sss21.slct)

#revert to data frame to make variables and detect values for each variable
df_sss21<-as.data.frame(sss21.slct)
str(df_sss21)
names(df_sss21)

rm(sss21.slct)
rm(sss21)


#======merge 2021 student background data========
#check primary key
df_sss21 %>% group_by(pc_id) %>% summarise(n_per_key=n()) %>% ungroup %>% count(n_per_key)

#-----------datamart file-----------------------
#read file
datamart21<- read_excel("Datamart21Fall.xlsx")
names(datamart21)
#select variables
df_datamart21 <- datamart21 %>% select (PC_ID, GENDER_CODE, ETHNICITY_REPORT_DESC, ADMIT_REGION, CITIZENSHIP, RESIDENT_YN, PARENTS_DEGREE, ADMIT_TYPE, TRANSFER_YN, COHORT, CLASS_LEVEL_RPT_LABEL, ACADEMIC_DEPT, CIP_CATEGORY, MAJOR_1, REG_PRIOR_CUM_GPA) %>% as.data.frame() #check
names(df_datamart21)<- tolower(names(df_datamart21))

# merge with data mart 
df_sss21_full<-left_join(df_sss21,df_datamart21,by = "pc_id")
#check
ncol(df_sss21_full)
ncol(df_sss21)

#--------------registar data-----------
#read data
reg21<- read_excel("Registar21Fall.xlsx")
names(reg21)
#select variables
df_reg21 <- reg21 %>% select (CumGPA, `FT/PT`, `People Code Id`) %>% as.data.frame() 
names(df_reg21)[3] <- "pc_id"
names(df_reg21)<- tolower(names(df_reg21))

# merge with registar data
df_sss21_full<-left_join(df_sss21_full,df_reg21, by = "pc_id")
#check
ncol(df_sss21_full)
ncol(df_sss21)

#remove unnecessary objects
rm(datamart21, df_datamart21, reg21, df_reg21, df_sss21)



#========merge historical data========
setwd("~/Documents/Rprojects/student_satisfaction_survey")
list.files()
source("sss_17-20_matched.R")  #run code from saved r script


#========save merged main files =====
#write_xlsx(df_sss21_full,"sss21_full.xlsx")
#write_xlsx(sss21.qual,"sss21_qual.xlsx")
#write_xlsx(df_sss_history,"sss17-20_matched.xlsx")

#save data in one excel
require(openxlsx)
list_of_datasets <- list("data_21quan" = df_sss21_full, "data_21qual" = sss21.qual, "data_17"=sss17.slct2, "data_18"=sss18.slct2, "data_19"=sss19.slct2, "data_20"=sss20.slct2)
write.xlsx(list_of_datasets, file = "fulldata.xlsx")

#check
list.files()
read_excel("fulldata.xlsx", sheet = "data_21quan")
read_excel("fulldata.xlsx", sheet = "data_21qual")
read_excel("fulldata.xlsx", sheet = "data_17")
read_excel("fulldata.xlsx", sheet = "data_18")
read_excel("fulldata.xlsx", sheet = "data_19")
read_excel("fulldata.xlsx", sheet = "data_20")