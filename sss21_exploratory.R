#============setting up==================
setwd("~/Documents/Rprojects/student_satisfaction_survey")
list.files()
source("data_cleaning_merging.R")  #run code from saved r script

#=========exploratory analysis=================
#
#use df_sss21_full to relate newly created col [29-55] to identity items [56-71]
names(df_sss21_full)


#------------------------------------------------------------------------
#