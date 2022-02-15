#setting up
setwd("~/Documents/Rprojects/data/student_satisfaction_survey")
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

sss21.slct<-sss21 %>% select(`Invite Custom Field 1`, contains(":Please rate")) 
names(sss21.slct) [1]<-"pc_id"
names(sss21.slct)<- gsub(":[A-Za-z]*.*$", "", names(sss21.slct))
names(sss21.slct)

#revert to data frame to make variables and detect values for each variable
df_sss21<-as.data.frame(sss21.slct)
str(df_sss21)
names(df_sss21)

#-----------------------------------------------------------------

# read datamart data file and rename
datamart21<- read_excel("Datamart21Fall.xlsx")
names(datamart21)
df_datamart21 <- datamart21 %>% select (PC_ID, GENDER_CODE, ETHNICITY_REPORT_DESC, ADMIT_REGION, CITIZENSHIP, RESIDENT_YN, PARENTS_DEGREE, ADMIT_TYPE, TRANSFER_YN, COHORT, CLASS_LEVEL_RPT_LABEL, ACADEMIC_DEPT, CIP_CATEGORY, MAJOR_1, REG_PRIOR_CUM_GPA) %>% as.data.frame() 
names(df_datamart21)<- tolower(names(df_datamart21))

# merge with data mart 
df_sss21_full<-left_join(df_sss21,df_datamart21,by.x = "pc_id", by.y = "pc_id")


# read registar data file and rename
reg21<- read_excel("Registar21Fall.xlsx")
names(reg21)
df_reg21 <- reg21 %>% select (CumGPA, `FT/PT`, `People Code Id`) %>% as.data.frame() 
names(df_reg21)[3] <- "pc_id"
names(df_reg21)<- tolower(names(df_reg21))

# merge with registar data
df_sss21_full<-left_join(df_sss21,df_reg21, by = "pc_id")

#---------------------------------------------------------------------------