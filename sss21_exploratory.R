

#----------------------setting up--------------------------------
setwd("~/Documents/Rprojects/student_satisfaction_survey")
list.files()
source("sss21_descriptive.R")  #run code from saved r script

#-------------------------merge data-------------------------------
# read datamart data file and rename
datamart21<- read_excel("Datamart21Fall.xlsx")
names(datamart21)
df_datamart21 <- datamart21 %>% select (PC_ID, GENDER_CODE, ETHNICITY_REPORT_DESC, ADMIT_REGION, CITIZENSHIP, RESIDENT_YN, PARENTS_DEGREE, ADMIT_TYPE, TRANSFER_YN, COHORT, CLASS_LEVEL_RPT_LABEL, ACADEMIC_DEPT, CIP_CATEGORY, MAJOR_1, REG_PRIOR_CUM_GPA) %>% as.data.frame() 
names(df_datamart21)<- tolower(names(df_datamart21))

# merge with data mart 
df_sss21_full<-left_join(df_sss21,df_datamart21,by.x = "pc_id", by.y = "pc_id")
ncol(df_sss21_full)
ncol(df_sss21)

# read registar data file and rename
reg21<- read_excel("Registar21Fall.xlsx")
names(reg21)
df_reg21 <- reg21 %>% select (CumGPA, `FT/PT`, `People Code Id`) %>% as.data.frame() 
names(df_reg21)[3] <- "pc_id"
names(df_reg21)<- tolower(names(df_reg21))

# merge with registar data
df_sss21_full<-left_join(df_sss21_full,df_reg21, by = "pc_id")
ncol(df_sss21_full)
ncol(df_sss21)


#===========================================exploratory analysis=================================================
#=============================================================================================

#use df_sss21_full to relate newly created col [29-55] to identity items [56-71]
names(df_sss21_full)


#------------------------------------------------------------------------
#